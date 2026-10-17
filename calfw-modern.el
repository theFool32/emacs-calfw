;;; calfw-modern.el --- Compact cards and time blocks for calfw -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A renderer for the original cfw: API.  The source-coloured time blocks
;; and two-column fortnight layout are inspired by haji-ali/maccalfw and
;; haji-ali/calfw-blocks (including calfw-transpose).  This implementation
;; keeps the existing models, Org markers, date navigation and detail view.

;;; Code:

(require 'calfw)
(require 'color)
(require 'subr-x)

(defgroup cfw-modern nil "Calendar cards and time blocks." :group 'cfw)

(defcustom cfw:modern-day-start 8
  "First hour normally shown in time views.
The range expands to include earlier and later appointments."
  :type '(integer :tag "Hour") :group 'cfw-modern)

(defcustom cfw:modern-day-end 20
  "Last hour normally shown in time views."
  :type '(integer :tag "Hour") :group 'cfw-modern)

(defface cfw:modern-title
  '((t :inherit default :weight bold :height 1.0))
  "Calendar heading, with the same line height as the grid." :group 'cfw-modern)
(defface cfw:modern-muted
  '((t :inherit shadow :height 1.0)) "Secondary calendar text." :group 'cfw-modern)
(defface cfw:modern-rule
  '((((background dark)) :foreground "#42484c")
    (t :foreground "#dce1e5")) "Quiet calendar separators." :group 'cfw-modern)
(defface cfw:modern-day
  '((t :inherit default :weight bold :height 1.0)) "Day headings." :group 'cfw-modern)
(defface cfw:modern-weekend
  '((t :inherit shadow :weight bold :height 1.0)) "Weekend headings." :group 'cfw-modern)
(defface cfw:modern-today
  '((((background dark)) :foreground "#f6c177" :weight bold)
    (t :foreground "#a84c1b" :weight bold)) "Today's date." :group 'cfw-modern)
(defface cfw:modern-button
  '((t :inherit default :weight normal :height 1.0)) "Toolbar buttons." :group 'cfw-modern)
(defface cfw:modern-active-button
  '((((background dark)) :foreground "#bdd8f1" :background "#394650" :weight bold)
    (t :foreground "#245578" :background "#deebf5" :weight bold))
  "Active calendar view." :group 'cfw-modern)
(defface cfw:modern-overflow
  '((((background dark)) :foreground "#f6c177" :weight bold)
    (t :foreground "#9b4b0b" :weight bold))
  "Additional items that can be opened in the complete day details."
  :group 'cfw-modern)

(defvar-local cfw:modern--dimensions nil)
(defvar-local cfw:modern--pixel-dimensions nil)
(defvar-local cfw:modern--line-height nil)
(defvar-local cfw:modern--rendering nil)
(defvar-local cfw:modern--resize-timer nil)

(defun cfw:modern--blend (front back weight)
  "Blend FRONT into BACK with WEIGHT between zero and one."
  (let ((f (or (color-name-to-rgb front) '(0.5 0.5 0.5)))
        (b (or (color-name-to-rgb back) '(0.15 0.15 0.15))))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (x y) (+ (* weight x) (* (- 1 weight) y))) f b)
                   '(2)))))

(defun cfw:modern--background ()
  "Return the frame background, including terminal fallbacks."
  (let ((bg (face-background 'default nil t)))
    (if (and (stringp bg) (color-defined-p bg)) bg
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#202428" "#ffffff"))))

(defun cfw:modern--source-color (source)
  "Return a legible colour for SOURCE in the current theme."
  (let ((color (or (and source (cfw:source-color source)) "#748faf")))
    (cfw:modern--blend color
                       (if (eq (frame-parameter nil 'background-mode) 'dark) "white" "black")
                       0.78)))

(defun cfw:modern--fit (text width &optional face)
  "Fit TEXT to exactly WIDTH columns, optionally using FACE."
  (let ((text (cfw:render-left (max 0 width) text)))
    (if face (cfw:rt text face) text)))

(defun cfw:modern--button (label command &optional active help)
  "Make a LABEL button invoking COMMAND; ACTIVE marks the selected view."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] command)
    (propertize (concat " " label " ") 'keymap map 'mouse-face 'highlight
                'help-echo (or help label)
                'face (if active 'cfw:modern-active-button 'cfw:modern-button))))

(defun cfw:modern--pack (items width)
  "Pack ITEMS into lines no wider than WIDTH, preserving their properties."
  (let (lines (line ""))
    (dolist (item items)
      (setq item (cfw:render-truncate item width t))
      (if (and (not (string-empty-p line))
               (> (+ (string-width line) 3 (string-width item)) width))
          (progn (push line lines) (setq line item))
        (setq line (concat line (unless (string-empty-p line) "   ") item))))
    (unless (string-empty-p line) (push line lines))
    (nreverse lines)))

(defun cfw:modern--legend (sources width)
  "Return wrapping SOURCE labels for WIDTH, with full names in tooltips."
  (let* ((short (mapcar (lambda (source)
                          (let ((name (cfw:source-name source)))
                            (if (equal name "org-agenda") "Org"
                              (car (last (split-string name " / ")))))) sources)))
    (cfw:modern--pack
     (cl-mapcar
      (lambda (source name)
        (when (> (cl-count name short :test #'equal) 1)
          (setq name (cfw:source-name source)))
        (propertize (concat "● " name) 'cfw:source source
                    'help-echo (cfw:source-name source)
                    'face (list :foreground (cfw:modern--source-color source))))
      sources short) width)))

(defun cfw:modern--header (component model width)
  "Return the header lines for COMPONENT and MODEL within WIDTH."
  (let* ((view (cfw:component-view component))
         (begin (cfw:k 'begin-date model)) (end (cfw:k 'end-date model))
         (prev (pcase view ('month #'cfw:navi-previous-month-command)
                      ('day #'cfw:navi-previous-day-command)
                      (_ #'cfw:navi-previous-week-command)))
         (next (pcase view ('month #'cfw:navi-next-month-command)
                      ('day #'cfw:navi-next-day-command)
                      (_ #'cfw:navi-next-week-command)))
         (title (if (eq view 'month)
                    (format "%d年 %d月" (nth 2 begin) (car begin))
                  (format "%d年 %d月%d日 — %s%d日"
                          (nth 2 begin) (car begin) (cadr begin)
                          (if (= (car begin) (car end)) "" (format "%d月" (car end)))
                          (cadr end))))
         (navigation (concat (cfw:modern--button "‹" prev nil "上一页")
                             (cfw:modern--button "今天" #'cfw:navi-goto-today-command)
                             (cfw:modern--button "›" next nil "下一页")))
         (views (mapconcat
                 (lambda (spec) (cfw:modern--button (nth 1 spec) (nth 2 spec)
                                                   (eq view (car spec))))
                 '((day "日" cfw:change-view-day) (week "周" cfw:change-view-week)
                   (two-weeks "双周" cfw:change-view-two-weeks)
                   (month "月" cfw:change-view-month)) " ")))
    (append (list (cfw:modern--fit title width 'cfw:modern-title)
                  (cfw:render-add-right width navigation views))
            (cfw:modern--legend (cfw:model-get-contents-sources model) width)
            (list (cfw:rt (make-string width ?─) 'cfw:modern-rule)))))

(defun cfw:modern--minutes (time)
  "Convert an (HOUR MINUTE) TIME to minutes."
  (when time (+ (* 60 (car time)) (cadr time))))

(defun cfw:modern--entry (item date &optional begin end)
  "Describe ITEM on DATE; BEGIN and END indicate a multi-day period."
  (let* ((event (cfw:event-p item))
         (text (if event (copy-sequence (cfw:event-title item)) (copy-sequence item)))
         (source (if event (cfw:event-source item) (get-text-property 0 'cfw:source text)))
         (hhmm (get-text-property 0 'time-of-day text))
         (start (if event (cfw:modern--minutes (cfw:event-start-time item))
                  (and hhmm (+ (* (/ hhmm 100) 60) (% hhmm 100)))))
         (finish (and event (cfw:modern--minutes (cfw:event-end-time item))))
         (duration (get-text-property 0 'duration text))
         (done (member (get-text-property 0 'todo-state text)
                       (and (boundp 'org-done-keywords-for-agenda) org-done-keywords-for-agenda))))
    (when (and start (not event))
      (let ((time (get-text-property 0 'time text)))
        (when (and (stringp time) (string-match "-\\([0-9][0-9]\\):\\([0-9][0-9]\\)" time))
          (setq finish (+ (* 60 (string-to-number (match-string 1 time)))
                          (string-to-number (match-string 2 time)))))))
    (when start
      (setq finish (or finish (+ start (if (and (numberp duration) (> duration 0)) duration 60))))
      (when (and begin (not (equal date begin))) (setq start 0))
      (when (and end (not (equal date end))) (setq finish 1440))
      (when (<= finish start) (setq finish 1440)))
    ;; Keep Org markers and link bindings; replace only presentation attributes.
    (remove-list-of-text-properties 0 (length text) '(face font-lock-face display invisible) text)
    (setq text (string-trim (replace-regexp-in-string "[\n\r]+" " " text)))
    (let* ((detail (if event (cfw:event-detail item) (copy-sequence item)))
           (map (get-text-property 0 'keymap text))
           (command (and (keymapp map) (lookup-key map (kbd "RET")))))
      (when begin
        (setq detail (concat (cfw:strtime begin) "–" (cfw:strtime end) "  " detail)))
      (list :text text :source source :start start :end finish :period (and begin t)
            :begin begin :finish-date end
            :location (if event (cfw:event-location item) (get-text-property 0 'cfw:org-loc text))
            :description (and event (cfw:event-description item))
            :command command
            :done done :help detail :detail detail))))

(defun cfw:modern--entries (model date)
  "Collect DATE's entries in MODEL, with appointments in time order."
  (let ((items (mapcar (lambda (item) (cfw:modern--entry item date))
                       (cfw:model-get-contents-by-date date model))))
    (dolist (period (cfw:k 'periods model))
      (pcase-let ((`(,begin ,end ,item) period))
        (when (cfw:date-between begin end date)
          (push (cfw:modern--entry item date begin end) items))))
    (setq items (cl-stable-sort
                 items (lambda (a b)
                         (let ((sa (plist-get a :start)) (sb (plist-get b :start)))
                           (cond ((not (eq (not (plist-get a :done)) (not (plist-get b :done))))
                                  (not (plist-get a :done)))
                                 ((and sa sb) (< sa sb)) (sa nil) (sb t)
                                 (t (funcall (cfw:model-get-sorter model)
                                             (plist-get a :text) (plist-get b :text))))))))
    (cl-loop for item in items for index from 0
             collect (plist-put item :index index))))

(defun cfw:modern--label (entry &optional compact)
  "Return ENTRY's label with a time or period marker.
COMPACT uses just the start time; the symbol `title' omits the time."
  (let* ((text (plist-get entry :text)) (start (plist-get entry :start))
         (end (plist-get entry :end))
         (label (if start
                    (cond ((eq compact 'title) "")
                          (compact (format "%02d:%02d " (/ start 60) (% start 60)))
                          (t (format "%02d:%02d–%02d:%02d " (/ start 60) (% start 60) (/ end 60) (% end 60))))
                  (if (plist-get entry :period) "↔ " ""))))
    ;; Org summaries already contain the start time.
    (when (and start (string-match "\\`[0-9][0-9]:[0-9][0-9] +" text))
      (setq text (substring text (match-end 0))))
    (concat label text)))

(defun cfw:modern--entry-line (entry text width date)
  "Render ENTRY's TEXT at WIDTH on DATE, keeping its navigation properties."
  (let* ((source (plist-get entry :source))
         (color (cfw:modern--source-color source))
         (background (cfw:modern--blend color (cfw:modern--background) 0.13))
         (text (cfw:modern--fit text (max 0 (1- width))))
         (props (cfw:extract-text-props (plist-get entry :text)
                                       'face 'font-lock-face 'display 'invisible))
         (line (apply #'propertize (concat "▎" text) props)))
    (add-text-properties
     0 (length line)
     (list 'face (list :background background :foreground color
                       :strike-through (and (plist-get entry :done) t))
           'cfw:date date 'cfw:row-count (plist-get entry :index)
           'cfw:source source 'help-echo (plist-get entry :help) 'mouse-face 'highlight) line)
    line))

(defun cfw:modern--more-click (event)
  "Show the full day behind a collapsed group clicked with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (cfw:show-details-command))

(defun cfw:modern--more (number width date)
  "Render a clickable NUMBER of additional entries at WIDTH on DATE."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cfw:modern--more-click)
    (dolist (key '("RET" "SPC" "s"))
      (define-key map (kbd key) #'cfw:show-details-command))
    (propertize (cfw:modern--fit (if (< width 10) (format "+%d" number) (format "+%d 项 ›" number)) width)
                'cfw:date date 'cfw:overflow-count number
                'face 'cfw:modern-overflow 'mouse-face 'highlight
                'help-echo (format "%d 项未展开；点击或按 RET / s / SPC 查看当天全部事项" number)
                'keymap map)))

(defun cfw:modern--entry-lines (entries date width capacity &optional compact)
  "Fit ENTRIES on DATE into CAPACITY lines of WIDTH.
Use single-line summaries when COMPACT or crowded. Always reserve room for
an exact overflow count. Roomy cards may use two lines per item."
  (let ((compact (or compact
                     (> (cl-loop for entry in entries
                                 sum (if (> (string-width (cfw:modern--label entry)) (max 1 (1- width))) 2 1))
                        capacity)))
        (remaining capacity) (shown 0) lines)
    (dolist (entry entries)
      (let ((space (- remaining (if (< (1+ shown) (length entries)) 1 0))))
        (when (> space 0)
          (let* ((text (cfw:modern--label entry compact))
                 (wrapped (if compact (list text)
                            (cfw:render-line-breaker-simple text (max 1 (1- width)) (min 2 space)))))
            (dolist (line wrapped)
              (push (cfw:modern--entry-line entry line width date) lines)
              (cl-decf remaining))
            (cl-incf shown)))))
    (when (and (< shown (length entries)) (> remaining 0))
      (push (cfw:modern--more (- (length entries) shown) width date) lines)
      (cl-decf remaining))
    (append (nreverse lines)
            (make-list (max 0 remaining) (propertize (make-string width ?\s) 'cfw:date date)))))

(defun cfw:modern--day-heading (date count width &optional compact)
  "Return a DATE heading with COUNT items, fitting WIDTH.
COMPACT uses just the day number for a monthly grid."
  (let* ((today (equal date (calendar-current-date)))
         (weekend (memq (calendar-day-of-week date) '(0 6)))
         (names ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])
         (title (cond ((eq compact 'timeline)
                       (format "%s %d%s" (aref names (calendar-day-of-week date)) (cadr date) (if today "·今" "")))
                      (compact (format " %d%s" (cadr date) (if today "·今" "")))
                      (t (format " %02d/%02d %s%s" (car date) (cadr date)
                          (aref names (calendar-day-of-week date)) (if today " · 今天" "")))))
         (heading (cfw:render-add-right width title (if (> count 0) (format "%d 项 " count) ""))))
    (propertize heading 'cfw:date date 'cfw:modern-header t
                'face (cond (today 'cfw:modern-today)
                            (weekend 'cfw:modern-weekend) (t 'cfw:modern-day)))))

(defun cfw:modern--card (model date width height &optional compact)
  "Return HEIGHT lines of a day card for MODEL and DATE at WIDTH."
  (let* ((entries (cfw:modern--entries model date))
         (capacity (max 0 (1- height))))
    (append (list (cfw:modern--day-heading date (length entries) width compact))
            (cfw:modern--entry-lines entries date width capacity compact))))

(defun cfw:modern--cards (model rows width height &optional compact)
  "Insert ROWS of day cards within WIDTH and HEIGHT for MODEL."
  (let* ((columns (length (car rows))) (gap "  │  ")
         (cell-width (max 1 (/ (- width (* (1- columns) (string-width gap))) columns)))
         (row-count (length rows))
         (card-lines (max (* 2 row-count) (- height (1- row-count)))))
    (cl-loop for row in rows for row-number from 0 do
             (let* ((cell-height (+ (/ card-lines row-count)
                                    (if (< row-number (% card-lines row-count)) 1 0)))
                    (cards (mapcar (lambda (date) (cfw:modern--card model date cell-width cell-height compact)) row)))
               (dotimes (line cell-height)
                 (insert (mapconcat (lambda (card) (nth line card)) cards
                                    (cfw:rt (copy-sequence gap) 'cfw:modern-rule)) "\n")))
             (unless (= row-number (1- (length rows)))
               (insert (cfw:rt (make-string width ?─) 'cfw:modern-rule) "\n")))))

(defun cfw:modern--month-weekdays (model width)
  "Return weekday headings aligned with MODEL's month grid at WIDTH."
  (let* ((days (cfw:k 'headers model))
         (gap "  │  ")
         (cell-width (max 1 (/ (- width (* (1- (length days)) (string-width gap)))
                               (length days))))
         (names ["周日" "周一" "周二" "周三" "周四" "周五" "周六"]))
    (mapconcat (lambda (day)
                 (cfw:modern--fit (concat " " (aref names day)) cell-width
                                  (if (memq day '(0 6)) 'cfw:modern-weekend 'cfw:modern-day)))
               days (cfw:rt (copy-sequence gap) 'cfw:modern-rule))))

(defun cfw:modern--lanes (entries first-minute lines-per-hour)
  "Assign ENTRIES to nonoverlapping display lanes.
FIRST-MINUTE and LINES-PER-HOUR specify the time-to-row mapping."
  (let (group result (group-end -1))
    (cl-labels
        ((finish-group ()
           (when group
             (let (ends placed)
               (dolist (entry (nreverse group))
                 (let* ((row (plist-get entry :row))
                        (lane (or (cl-position-if (lambda (end) (<= end row)) ends) (length ends))))
                   (if (= lane (length ends)) (setq ends (append ends (list (plist-get entry :last-row))))
                     (setf (nth lane ends) (plist-get entry :last-row)))
                   (push (plist-put entry :lane lane) placed)))
               (dolist (entry placed)
                 (push (plist-put entry :lanes (length ends)) result))))
           (setq group nil group-end -1)))
      (dolist (entry (cl-stable-sort (copy-sequence entries)
                                     (lambda (a b) (< (plist-get a :start) (plist-get b :start)))))
        (let* ((entry (copy-sequence entry))
               (row (floor (* (- (plist-get entry :start) first-minute) (/ lines-per-hour 60.0))))
               (last-row (max (+ row (if (< lines-per-hour 2) 1 2)) (ceiling (* (- (plist-get entry :end) first-minute)
                                                                   (/ lines-per-hour 60.0))))))
          (when (>= row group-end) (finish-group))
          (setq entry (plist-put entry :row row))
          (setq entry (plist-put entry :last-row last-row))
          (setq group-end (max group-end last-row))
          (push entry group)))
      (finish-group))
    (nreverse result)))

(defun cfw:modern--time-column (entries date width rows first-minute lines-per-hour)
  "Return a timeline column for ENTRIES on DATE.
WIDTH, ROWS, FIRST-MINUTE and LINES-PER-HOUR define its geometry."
  (let* ((entries (cfw:modern--lanes entries first-minute lines-per-hour))
         (lines (make-vector rows nil)))
    (dotimes (row rows)
      (let* ((active (cl-remove-if-not (lambda (entry) (and (<= (plist-get entry :row) row)
                                                           (< row (plist-get entry :last-row)))) entries))
             (total-lanes (if active (plist-get (car active) :lanes) 1))
             (lanes (min total-lanes (max 1 (/ width 4))))
             (cell-width (max 1 (/ width lanes)))
             (text ""))
        (dotimes (lane lanes)
          (let* ((entry (cl-find lane active :key (lambda (entry) (plist-get entry :lane))))
                 (w (if (= lane (1- lanes)) (- width (* lane cell-width)) cell-width)))
            (setq text
                  (concat text
                          (cond
                           ((and (> total-lanes lanes) (= lane (1- lanes)))
                            (let ((hidden (cl-count-if (lambda (item) (>= (plist-get item :lane) lane)) active)))
                              (if (> hidden 0) (cfw:modern--more hidden w date) (make-string w ?\s))))
                           (entry
                              (let* ((offset (- row (plist-get entry :row)))
                                     (label (cfw:modern--label entry (cond ((< w 12) 'title) ((< w 28) t))))
                                     (wrapped (cfw:render-line-breaker-simple label (max 1 (1- w))
                                                                            (- (plist-get entry :last-row) (plist-get entry :row))))
                                     (line (or (nth offset wrapped) "")))
                                (cfw:modern--entry-line entry line w date)))
                           (t (propertize (make-string w ?\s) 'cfw:date date)))))))
        (when (or (= row 0)
                  (/= (floor (/ row (float lines-per-hour)))
                      (floor (/ (1- row) (float lines-per-hour)))))
          (add-face-text-property 0 (length text) '(:overline t) t text))
        (aset lines row text)))
    (append lines nil)))

(defun cfw:modern--hidden-count (entries lines)
  "Count ENTRIES that never appear in rendered LINES, once per item."
  (let (shown)
    (dolist (line lines)
      (let ((pos 0))
        (while (< pos (length line))
          (when-let* ((index (get-text-property pos 'cfw:row-count line)))
            (cl-pushnew index shown))
          (setq pos (next-single-property-change pos 'cfw:row-count line (length line))))))
    (cl-count-if-not (lambda (entry) (memq (plist-get entry :index) shown)) entries)))

(defun cfw:modern--timeline (model days width height)
  "Insert source-coloured time blocks for DAYS in MODEL at WIDTH and HEIGHT."
  (let* ((gutter 6) (gap " │ ") (columns (length days))
         (cell-width (max 1 (/ (- width gutter (* (1- columns) (string-width gap))) columns)))
         (all-entries (mapcar (lambda (date) (cfw:modern--entries model date)) days))
         (timed (mapcar (lambda (entries) (cl-remove-if-not (lambda (entry) (plist-get entry :start)) entries)) all-entries))
         (untimed (mapcar (lambda (entries) (cl-remove-if (lambda (entry) (plist-get entry :start)) entries)) all-entries))
         (flat (apply #'append timed))
         (first-hour (max 0 (min cfw:modern-day-start
                                 (if flat (/ (apply #'min (mapcar (lambda (entry) (plist-get entry :start)) flat)) 60)
                                   cfw:modern-day-start))))
         (last-hour (min 24 (max cfw:modern-day-end
                                 (if flat (ceiling (/ (apply #'max (mapcar (lambda (entry) (plist-get entry :end)) flat)) 60.0))
                                   cfw:modern-day-end))))
         (task-lines (min 4 (apply #'max 0 (mapcar #'length untimed))))
         (hours (max 1 (- last-hour first-hour)))
         ;; Reserve the heading, divider and a per-day overflow summary.
         ;; Short windows compress the time axis instead of losing late events.
         (available-rows (max 1 (- height task-lines 3)))
         (lines-per-hour (/ (float available-rows) hours))
         (rows available-rows)
         (task-columns (cl-mapcar (lambda (entries date)
                                   (cfw:modern--entry-lines entries date cell-width task-lines t))
                                 untimed days)))
    (insert (make-string gutter ?\s)
            (mapconcat (lambda (pair) (cfw:modern--day-heading (car pair) (length (cdr pair)) cell-width 'timeline))
                       (cl-mapcar #'cons days all-entries) (cfw:rt (copy-sequence gap) 'cfw:modern-rule)) "\n")
    (dotimes (row task-lines)
      (insert (cfw:modern--fit (if (= row 0) "全天" "") gutter 'cfw:modern-muted))
      (insert (mapconcat
               (lambda (lines) (nth row lines)) task-columns
               (cfw:rt (copy-sequence gap) 'cfw:modern-rule)) "\n"))
    (insert (cfw:rt (make-string width ?─) 'cfw:modern-rule) "\n")
    (let* ((day-lines (cl-mapcar (lambda (entries date)
                                 (cfw:modern--time-column entries date cell-width rows (* first-hour 60) lines-per-hour))
                               timed days))
           (hidden (cl-mapcar #'cfw:modern--hidden-count timed day-lines)))
      (dotimes (row rows)
        (insert (cfw:modern--fit (if (or (= row 0)
                                       (/= (floor (/ row (float lines-per-hour)))
                                           (floor (/ (1- row) (float lines-per-hour)))))
                                   (format "%02d:00" (+ first-hour (floor (/ row (float lines-per-hour))))) "")
                               gutter 'cfw:modern-muted)
                (mapconcat (lambda (lines) (nth row lines)) day-lines
                           (cfw:rt (copy-sequence gap) 'cfw:modern-rule)) "\n"))
      (when (cl-some (lambda (count) (> count 0)) hidden)
        (insert (cfw:modern--fit "更多" gutter 'cfw:modern-muted)
                (mapconcat (lambda (pair)
                             (if (> (cdr pair) 0) (cfw:modern--more (cdr pair) cell-width (car pair))
                               (propertize (make-string cell-width ?\s) 'cfw:date (car pair))))
                           (cl-mapcar #'cons days hidden) (cfw:rt (copy-sequence gap) 'cfw:modern-rule)) "\n")))))

(defun cfw:modern-details-layout (date model)
  "Return every item on DATE in MODEL, in the same order as the overview.
Full titles, times, locations and descriptions retain their source bindings."
  (let ((entries (cfw:modern--entries model date))
        (holiday (cfw:model-get-holiday-by-date date model))
        (annotation (cfw:model-get-annotation-by-date date model)))
    (concat (format "* %s · %s · %d 项\n" (cfw:strtime date)
                    (calendar-day-name date) (length entries))
            (when (or holiday annotation)
              (concat (mapconcat #'identity (delq nil (list holiday annotation)) " / ") "\n"))
            "\n"
            (mapconcat
             (lambda (entry)
               (let* ((source (plist-get entry :source))
                      (location (plist-get entry :location))
                      (description (plist-get entry :description))
                      (begin (plist-get entry :begin))
                      (heading (apply #'propertize (concat "** " (cfw:modern--label entry))
                                      (cfw:extract-text-props (plist-get entry :text)
                                                             'face 'font-lock-face 'display 'invisible))))
                 (propertize
                  (concat heading "\n"
                          (when source
                            (concat "来源：" (cfw:source-name source) "\n"))
                          (when begin
                            (format "日期：%s — %s\n" (cfw:strtime begin)
                                    (cfw:strtime (plist-get entry :finish-date))))
                          (when (and location (not (string-empty-p location)))
                            (concat "地点：" (replace-regexp-in-string "[\n\r]+" " " location) "\n"))
                          (when (and description (not (string-empty-p description)))
                            (concat "\n" (replace-regexp-in-string "^" "  " description) "\n")))
                  'cfw:date date 'cfw:row-count (plist-get entry :index)
                  ;; Org fontification owns `keymap'; keep the source action in
                  ;; our own property so RET works after highlighting and folding.
                  'cfw:source-command (plist-get entry :command))))
             entries "\n\n") "\n")))

(defun cfw:modern--resized-p (window)
  "Whether WINDOW's character or pixel dimensions changed."
  (or (not (equal cfw:modern--dimensions (cons (window-body-width window) (window-body-height window))))
      (not (equal cfw:modern--pixel-dimensions (cons (window-body-width window t) (window-body-height window t))))))

(defun cfw:modern-refit-buffer (buffer)
  "Refit BUFFER immediately after a popup or other window change."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp cfw:modern--resize-timer) (cancel-timer cfw:modern--resize-timer))
      (setq cfw:modern--resize-timer nil)
      (when-let* ((window (get-buffer-window buffer t))
                  (component (and (not cfw:modern--rendering)
                                  (eq cfw:display-style 'modern)
                                  (local-variable-p 'cfw:component)
                                  (buffer-local-value 'cfw:component buffer))))
        (when (cfw:modern--resized-p window)
          (with-selected-window window (cfw:cp-update component)))))))

(defun cfw:modern--resize (frame)
  "Schedule a refit of visible calendars in FRAME."
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (and (eq major-mode 'cfw:calendar-mode) (eq cfw:display-style 'modern)
                 (not cfw:modern--resize-timer)
                 (cfw:modern--resized-p window))
        (setq cfw:modern--resize-timer
              (run-at-time 0.01 nil #'cfw:modern-refit-buffer (current-buffer)))))))

(defun cfw:modern-render (component)
  "Render COMPONENT using compact cards and time blocks."
  (let* ((cfw:modern--rendering t)
         (dest (cfw:component-dest component)) (view (cfw:component-view component))
         (window (and (eq (cfw:dest-type dest) 'buffer) (get-buffer-window (current-buffer) t)))
         (width (max 20 (1- (if window (window-body-width window) (cfw:dest-width dest)))))
         ;; CJK glyphs can be taller than frame-char-height. Reuse their measured
         ;; height, then verify the rendered pixels before accepting the layout.
         (height (max 12 (if window
                                (floor (/ (window-body-height window t)
                                          (float (or cfw:modern--line-height (frame-char-height (window-frame window))))))
                              (cfw:dest-height dest))))
         (model (pcase view ('month (cfw:view-month-model (cfw:component-model component)))
                       ('two-weeks (cfw:view-two-weeks-model (cfw:component-model component)))
                       ('week (cfw:view-week-model (cfw:component-model component)))
                       (_ (let ((date (cfw:k 'init-date (cfw:component-model component))))
                            (cfw:view-model-make-common-data-for-days (cfw:component-model component) date date)))))
         (header (append (cfw:modern--header component model width)
                         (when (and (eq view 'month) (>= width 70))
                           (list (cfw:modern--month-weekdays model width)))))
         (body-height (max 4 (- height (length header) 2)))
         (begin (point)) (attempts 0) (height-limit most-positive-fixnum) (retry t))
    (setf (cfw:component-model component) model)
    (setq-local cfw:navigation-layout
                (when (eq view 'two-weeks)
                  (if (>= width 70) 'two-columns 'single-column)))
    (when window
      (setq-local line-spacing 0)
      (setq cfw:modern--dimensions (cons (window-body-width window) (window-body-height window))
            cfw:modern--pixel-dimensions (cons (window-body-width window t) (window-body-height window t))))
    (while retry
      (delete-region begin (point))
      (setq body-height (max 4 (- height (length header) 2)))
      (dolist (line header) (insert line "\n"))
      (cond
       ((and (memq view '(day week)) (>= width 70))
        (cfw:modern--timeline model (if (eq view 'week) (car (cfw:k 'weeks model)) (cfw:k 'days model)) width body-height))
       ((and (eq view 'two-weeks) (>= width 70))
        (cfw:modern--cards model (cl-mapcar #'list (car (cfw:k 'weeks model)) (cadr (cfw:k 'weeks model))) width body-height))
       ((and (eq view 'month) (>= width 70))
        (cfw:modern--cards model (cfw:k 'weeks model) width body-height t))
       (t
        ;; Narrow split windows use full-width day cards instead of squeezed grids.
        (cfw:modern--cards model (mapcar #'list (cfw:enumerate-days (cfw:k 'begin-date model) (cfw:k 'end-date model)))
                           width body-height)))
      (insert (cfw:rt (make-string width ?─) 'cfw:modern-rule) "\n"
              (cfw:modern--fit " s 查看详情（详情内 RET 跳转原任务）   r 刷新   t 今天   D/W/T/M 切换视图" width 'cfw:modern-muted))
      (setq retry nil)
      (when window
        (let* ((pixels (cdr (window-text-pixel-size window begin (point))))
               (rows (max 1 (count-lines begin (point))))
               (available (window-body-height window t)))
          (setq cfw:modern--line-height (max (frame-char-height (window-frame window))
                                             (/ (float pixels) rows)))
          (when (> pixels available) (setq height-limit (1- height)))
          ;; Use the measured spare pixels, including partial hours and card
          ;; rows. A failed height is an upper bound, preventing resize loops.
          (let* ((delta (floor (/ (- available pixels) cfw:modern--line-height)))
                 (target (max 12 (min height-limit (+ height delta)))))
            (when (and (/= target height) (< attempts 4))
              (setq height target retry t)
              (cl-incf attempts)))
          (when (<= pixels available)
            (set-window-start window (point-min) t)
            (set-window-vscroll window 0)))))))

(add-hook 'window-size-change-functions #'cfw:modern--resize)

(provide 'calfw-modern)
;;; calfw-modern.el ends here

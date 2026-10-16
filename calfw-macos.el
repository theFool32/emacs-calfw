;;; calfw-macos.el --- macOS Calendar sources for calfw -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Read system calendars through EventKit, following maccalfw's bridge design:
;; https://github.com/haji-ali/maccalfw
;; This bridge uses a small, read-only Swift helper and the existing cfw: API.
;; No calendar permission is requested automatically.  Run
;; `cfw:macos-request-access', then customize `cfw:macos-calendars'.

;;; Code:
(require 'calfw)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup cfw-macos nil
  "Display macOS Calendar events in calfw."
  :group 'cfw)

(defconst cfw:macos--source-directory
  (file-name-directory
   (file-truename (or (locate-library "calfw-macos.el") load-file-name
                      buffer-file-name)))
  "Directory of the real source file, including with symlink-based installers.")

(defconst cfw:macos--selector-type
  '(choice (string :tag "Calendar title")
           (cons :tag "Account and title" (string :tag "Account")
                 (string :tag "Calendar title"))
           (list :tag "Calendar ID" (const :id) string)))

(defcustom cfw:macos-calendars nil
  "System calendars to display alongside Org.
Nil disables the integration.  t selects all calendars, subject to
`cfw:macos-excluded-calendars'.  Otherwise use a list of selectors:
  \"日程\"                 a calendar title (must be unambiguous)
  (\"iCloud\" . \"日程\")  an account and calendar title
  (:id \"identifier\")     a local calendar identifier.
Account/title selectors are portable across computers.  Use
`cfw:macos-list-calendars' to inspect names and identifiers.  Reopen the
calendar after changing selection.  No source events are modified."
  :group 'cfw-macos
  :type `(choice (const :tag "Disabled" nil)
                 (const :tag "All calendars except exclusions" t)
                 (repeat :tag "Selected calendars" ,cfw:macos--selector-type)))

(defcustom cfw:macos-excluded-calendars '("Org")
  "Calendars to exclude even if selected in `cfw:macos-calendars'.
Selectors have the same format as `cfw:macos-calendars'.  By default,
system calendars named Org are excluded, so their synchronized copies
do not duplicate calfw's local Org source.  This filters display only."
  :group 'cfw-macos
  :type `(repeat ,cfw:macos--selector-type))

(defcustom cfw:macos-helper-directory
  (expand-file-name "calfw-macos/" user-emacs-directory)
  "Local build directory for the signed EventKit helper application.
Builds stay outside the package checkout and are specific to each Mac."
  :group 'cfw-macos :type 'directory)

(defcustom cfw:macos-cache-seconds 60
  "Seconds to reuse a system calendar query.
Refreshing calfw clears the cache immediately."
  :group 'cfw-macos :type 'natnum)

(defcustom cfw:macos-query-timeout 15
  "Maximum seconds to wait for a system calendar query."
  :group 'cfw-macos :type 'number)

(defvar cfw:macos--access-pending nil)
(defvar cfw:macos--event-cache (make-hash-table :test #'equal))

(defun cfw:macos--helper ()
  "Return the helper executable path."
  (expand-file-name "CalfwCalendar.app/Contents/MacOS/calfw-calendar"
                    cfw:macos-helper-directory))

(defun cfw:macos-build-helper ()
  "Compile and sign the local read-only EventKit helper.
Requires Apple's Command Line Tools.  Does not request calendar access."
  (interactive)
  (unless (eq system-type 'darwin) (user-error "macOS calendars require macOS"))
  (let* ((swift (or (executable-find "swiftc")
                    (user-error "Install Apple's Command Line Tools (xcode-select --install)")))
         (codesign (or (executable-find "codesign") (user-error "codesign is unavailable")))
         (source (expand-file-name "calfw-macos.swift" cfw:macos--source-directory))
         (binary (cfw:macos--helper))
         (contents (file-name-directory (directory-file-name (file-name-directory binary))))
         (buffer (get-buffer-create "*calfw macOS build*")))
    (unless (file-readable-p source)
      (user-error "Package installation is missing calfw-macos.swift; include it with calfw-macos.el"))
    (make-directory (file-name-directory binary) t)
    (with-temp-file (expand-file-name "Info.plist" contents)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
              "<plist version=\"1.0\"><dict>\n"
              "<key>CFBundleIdentifier</key><string>org.calfw.calendar-helper</string>\n"
              "<key>CFBundleName</key><string>Calfw Calendar</string>\n"
              "<key>CFBundleExecutable</key><string>calfw-calendar</string>\n"
              "<key>CFBundlePackageType</key><string>APPL</string>\n"
              "<key>CFBundleVersion</key><string>1</string>\n"
              "<key>LSUIElement</key><true/>\n"
              "<key>NSCalendarsUsageDescription</key><string>Calfw reads selected calendars to display their events beside Org.</string>\n"
              "<key>NSCalendarsFullAccessUsageDescription</key><string>Calfw only reads selected calendar events. macOS requires full access to read events.</string>\n"
              "</dict></plist>\n"))
    (with-current-buffer buffer (erase-buffer))
    (unless (zerop (process-file swift nil buffer t "-O" "-module-cache-path"
                                 (expand-file-name "swift-cache" cfw:macos-helper-directory)
                                 source "-o" binary))
      (display-buffer buffer)
      (error "EventKit helper compilation failed; see *calfw macOS build*"))
    (unless (zerop (process-file codesign nil buffer t "--force" "--sign" "-"
                                 "--identifier" "org.calfw.calendar-helper"
                                 (file-name-directory (directory-file-name contents))))
      (display-buffer buffer)
      (error "EventKit helper signing failed; see *calfw macOS build*"))
    (when (called-interactively-p 'interactive) (message "Calfw EventKit helper built"))
    binary))

(defun cfw:macos--ensure-helper ()
  "Return the compiled helper, building it when necessary."
  (unless (eq system-type 'darwin) (user-error "macOS calendars require macOS"))
  (let ((binary (cfw:macos--helper))
        (source (expand-file-name "calfw-macos.swift" cfw:macos--source-directory)))
    (when (or (not (file-executable-p binary))
              (file-newer-than-file-p source binary))
      (cfw:macos-build-helper))
    binary))

(defun cfw:macos--command (stdout stderr arguments)
  "Return a LaunchServices command with STDOUT, STDERR and ARGUMENTS.
Launching the helper as an app gives it its own calendar permission identity,
including when Emacs itself was started as a background daemon."
  (cfw:macos--ensure-helper)
  (append (list "/usr/bin/open" "-g" "-j" "-n" "-a"
                (expand-file-name "CalfwCalendar.app" cfw:macos-helper-directory)
                "--stdout" stdout "--stderr" stderr "--args") arguments))

(defun cfw:macos--read-response (stdout stderr)
  "Read a helper response from STDOUT, using STDERR for errors."
  (with-temp-buffer
    (insert-file-contents stdout)
    (when (zerop (buffer-size))
      (error "Calfw macOS: %s"
             (let ((message (string-trim
                             (with-temp-buffer
                               (insert-file-contents stderr) (buffer-string)))))
               (if (string-empty-p message) "Calendar helper returned no data" message))))
    (goto-char (point-min))
    (let ((data (json-parse-buffer :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil)))
      (unless (equal (alist-get 'version data) 1)
        (error "Unsupported EventKit helper response; rebuild the helper"))
      data)))

(defun cfw:macos--response-ready-p (stdout stderr)
  "Return non-nil once the helper has written a complete response.
The helper ends each JSON response and each error with a newline."
  (cl-some (lambda (file)
             (and (file-exists-p file)
                  (> (file-attribute-size (file-attributes file)) 0)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (eq (char-before (point-max)) ?\n))))
           (list stdout stderr)))

(defun cfw:macos--wait-response (stdout stderr timeout)
  "Wait up to TIMEOUT seconds for STDOUT or STDERR, then decode it.
Do not use open -W: a short-lived app can exit before its kevent wait
is registered, yielding ESRCH even after the query succeeded."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (cfw:macos--response-ready-p stdout stderr))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (unless (cfw:macos--response-ready-p stdout stderr)
      (error "Calendar helper timed out after %s seconds; retry refreshing calfw" timeout))
    (cfw:macos--read-response stdout stderr)))

(defun cfw:macos--call (&rest arguments)
  "Run the helper with ARGUMENTS and decode its completed response."
  (let ((stdout (make-temp-file "calfw-macos-output-"))
        (stderr (make-temp-file "calfw-macos-error-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((command (cfw:macos--command stdout stderr arguments))
                 (status (apply #'process-file (car command) nil t nil (cdr command))))
            (unless (eq status 0)
              (error "Cannot launch calendar helper: %s" (string-trim (buffer-string))))
            (cfw:macos--wait-response stdout stderr cfw:macos-query-timeout)))
      (delete-file stdout)
      (delete-file stderr))))

(defun cfw:macos--access-finish (stdout stderr &optional launch-error)
  "Complete an access request using STDOUT and STDERR or LAUNCH-ERROR."
  (unwind-protect
      (condition-case err
          (progn
            (when launch-error (error "%s" launch-error))
            (unless (equal "full-access"
                           (alist-get 'authorization (cfw:macos--read-response stdout stderr)))
              (error "Calendar read access was not granted"))
            (message "Calendar access granted. Run M-x cfw:macos-list-calendars"))
        (error (display-warning 'cfw-macos (error-message-string err))))
    (setq cfw:macos--access-pending nil)
    (delete-file stdout)
    (delete-file stderr)))

(defun cfw:macos--check-access (stdout stderr deadline)
  "Check the asynchronous access response until DEADLINE."
  (cond ((cfw:macos--response-ready-p stdout stderr)
         (cfw:macos--access-finish stdout stderr))
        ((>= (float-time) deadline)
         (cfw:macos--access-finish stdout stderr "Calendar authorization timed out; retry cfw:macos-request-access"))
        (t (run-at-time 0.1 nil #'cfw:macos--check-access stdout stderr deadline))))

;;;###autoload
(defun cfw:macos-request-access ()
  "Request system calendar permission explicitly, without blocking Emacs.
macOS calls its reading permission full access.  The helper only reads events."
  (interactive)
  (when (or cfw:macos--access-pending (get-process "calfw-calendar-access"))
    (user-error "A calendar access request is already running"))
  (let* ((stdout (make-temp-file "calfw-macos-access-"))
         (stderr (make-temp-file "calfw-macos-error-"))
         (buffer (get-buffer-create "*calfw macOS access*"))
         (deadline (+ (float-time) 125)))
    (with-current-buffer buffer (erase-buffer))
    (setq cfw:macos--access-pending t)
    (condition-case err
        (make-process
         :name "calfw-calendar-access" :buffer buffer :noquery t
         :command (cfw:macos--command stdout stderr '("authorize"))
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (if (zerop (process-exit-status process))
                 (cfw:macos--check-access stdout stderr deadline)
               (cfw:macos--access-finish stdout stderr "Could not launch calendar authorization")))))
      (error (setq cfw:macos--access-pending nil)
             (delete-file stdout) (delete-file stderr)
             (signal (car err) (cdr err))))))

(defun cfw:macos--calendars ()
  "Return available system event calendars."
  (alist-get 'calendars (cfw:macos--call "calendars")))

;;;###autoload
(defun cfw:macos-list-calendars ()
  "Display system calendar accounts, titles and local identifiers."
  (interactive)
  (let ((calendars (cfw:macos--calendars)))
    (with-help-window "*calfw macOS calendars*"
      (princ "Set cfw:macos-calendars in M-x customize-group RET cfw-macos.\n")
      (princ "Calendar titles or (ACCOUNT . TITLE) pairs work across computers.\n")
      (princ "Reopen your calfw calendar after changing the selection.\n\n")
      (dolist (calendar calendars)
        (princ (format "%s / %s\n  ID: %s%s\n\n"
                       (alist-get 'source calendar) (alist-get 'title calendar)
                       (alist-get 'id calendar)
                       (if (cl-some (lambda (selector)
                                      (cfw:macos--matches-p selector calendar))
                                    cfw:macos-excluded-calendars)
                           "  [excluded]" "")))))))

(defun cfw:macos--matches-p (selector calendar)
  "Return non-nil if SELECTOR matches CALENDAR."
  (cond ((stringp selector) (equal selector (alist-get 'title calendar)))
        ((and (eq (car-safe selector) :id) (= (length selector) 2))
         (equal (cadr selector) (alist-get 'id calendar)))
        ((and (consp selector) (stringp (car selector)) (stringp (cdr selector)))
         (and (equal (car selector) (alist-get 'source calendar))
              (equal (cdr selector) (alist-get 'title calendar))))
        (t (error "Invalid calendar selector: %S" selector))))

(defun cfw:macos--selected-calendars (calendars)
  "Select CALENDARS according to inclusion and exclusion options."
  (let* ((available (cl-remove-if
                     (lambda (calendar)
                       (cl-some (lambda (selector) (cfw:macos--matches-p selector calendar))
                                cfw:macos-excluded-calendars)) calendars))
         (selection (if (eq cfw:macos-calendars t) available
                      (cl-loop for selector in cfw:macos-calendars append
                               (let ((matches (cl-remove-if-not
                                               (lambda (calendar)
                                                 (cfw:macos--matches-p selector calendar))
                                               calendars)))
                                 (when (> (length matches) 1)
                                   (user-error "Ambiguous calendar %S; select an account/title pair or ID" selector))
                                 (unless matches
                                   (user-error "Calendar %S not found; run cfw:macos-list-calendars" selector))
                                 (cl-intersection matches available :test #'equal))))))
    (cl-remove-duplicates selection :test #'equal)))

(defun cfw:macos-clear-cache ()
  "Clear cached system events, including when calfw refreshes."
  (interactive)
  (clrhash cfw:macos--event-cache))

(defun cfw:macos--date (time)
  "Return the calendar date of encoded TIME."
  (let ((decoded (decode-time time)))
    (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded))))

(defun cfw:macos--time (time)
  "Return the hour and minute of encoded TIME."
  (let ((decoded (decode-time time))) (list (nth 2 decoded) (nth 1 decoded))))

(defun cfw:macos--previous-date (date)
  "Return the calendar day before DATE, without assuming 24-hour days."
  (calendar-gregorian-from-absolute (1- (calendar-absolute-from-gregorian date))))

(defun cfw:macos--encode-date (date &optional next-day)
  "Encode DATE at midnight, or the following midnight when NEXT-DAY is non-nil."
  (encode-time 0 0 0 (+ (cadr date) (if next-day 1 0)) (car date) (nth 2 date)))

(defun cfw:macos--convert-event (record calendar)
  "Convert an EventKit RECORD in CALENDAR to a calfw event."
  (let* ((start (seconds-to-time (alist-get 'start record)))
         (end (seconds-to-time (alist-get 'end record)))
         (all-day (alist-get 'allDay record))
         (start-date (cfw:macos--date start))
         (end-date (cfw:macos--date end))
         (end-decoded (decode-time end))
         ;; EventKit ends are exclusive; calfw period end dates are inclusive.
         (last-date (if (and (time-less-p start end)
                             (or all-day (equal (cl-subseq end-decoded 0 3) '(0 0 0))))
                        (cfw:macos--previous-date end-date) end-date))
         (notes (alist-get 'notes record))
         (description (string-join
                       (delq nil (list (unless (string-empty-p (or notes "")) notes)
                                       (format "Calendar: %s / %s"
                                               (alist-get 'source calendar) (alist-get 'title calendar))
                                       (unless all-day
                                         (format "%s — %s"
                                                 (format-time-string "%F %R %Z" start)
                                                 (format-time-string "%F %R %Z" end))))) "\n\n"))
         (title (or (alist-get 'title record) "")))
    (make-cfw:event
     :title (if all-day title
              (propertize title 'time-of-day
                          (+ (* 100 (car (cfw:macos--time start)))
                             (cadr (cfw:macos--time start)))))
     :start-date start-date :start-time (unless all-day (cfw:macos--time start))
     :end-date (unless (equal start-date last-date) last-date)
     :end-time (unless all-day (cfw:macos--time end))
     :location (alist-get 'location record) :description description)))

(defun cfw:macos--events (begin end identifiers)
  "Read events for IDENTIFIERS from BEGIN through END, inclusive."
  (let* ((key (list begin end identifiers (current-time-zone)))
         (cached (gethash key cfw:macos--event-cache))
         (now (float-time)))
    (if (and cached (< (- now (car cached)) cfw:macos-cache-seconds)) (cdr cached)
      (let ((records (alist-get
                      'events
                      (apply #'cfw:macos--call "events"
                             (number-to-string (float-time (cfw:macos--encode-date begin)))
                             (number-to-string (float-time (cfw:macos--encode-date end t)))
                             identifiers))))
        (puthash key (cons now records) cfw:macos--event-cache)
        records))))

(defun cfw:macos--source-data (calendar identifiers begin end)
  "Return CALENDAR's calfw contents between BEGIN and END for IDENTIFIERS."
  (let (contents periods)
    (dolist (record (cfw:macos--events begin end identifiers))
      (when (equal (alist-get 'calendar record) (alist-get 'id calendar))
        (let ((event (cfw:macos--convert-event record calendar)))
          (if (cfw:event-end-date event) (push event periods) (push event contents)))))
    (append (nreverse contents) (when periods (list (cons 'periods (nreverse periods)))))))

(defun cfw:macos-create-sources ()
  "Return selected macOS sources, or nil when disabled or not on macOS.
Each calendar keeps its own name and color.  All sources share one cached
EventKit query per visible date range; refreshing clears that cache."
  (when (and (eq system-type 'darwin) cfw:macos-calendars)
    (let* ((calendars (cfw:macos--selected-calendars (cfw:macos--calendars)))
           (identifiers (mapcar (lambda (calendar) (alist-get 'id calendar)) calendars)))
      (mapcar (lambda (calendar)
                (make-cfw:source
                 :name (format "%s / %s" (alist-get 'source calendar) (alist-get 'title calendar))
                 :color (alist-get 'color calendar)
                 :update #'cfw:macos-clear-cache
                 :data (lambda (begin end)
                         (cfw:macos--source-data calendar identifiers begin end))))
              calendars))))

(provide 'calfw-macos)
;;; calfw-macos.el ends here

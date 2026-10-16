;;; calfw-macos-tests.el --- EventKit bridge regression tests -*- lexical-binding: t; -*-
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'calfw-macos)
(require 'calfw-org)
(setq native-comp-enable-subr-trampolines nil)

(defconst cfw-macos-test-calendars
  '(((id . "personal") (source . "iCloud") (title . "日程") (color . "#0000ff"))
    ((id . "org-copy") (source . "iCloud") (title . "Org") (color . "#00ff00"))
    ((id . "travel") (source . "iCloud") (title . "携程") (color . "#ff0000"))
    ((id . "work") (source . "caldav.feishu.cn") (title . "李杰") (color . "#0088ff"))))

(defun cfw-macos-test-event (start end &optional all-day calendar title)
  `((start . ,(float-time (encode-time (parse-time-string start))))
    (end . ,(float-time (encode-time (parse-time-string end))))
    (allDay . ,all-day) (calendar . ,(or calendar "personal"))
    (title . ,(or title "Meeting")) (notes . "Notes") (location . "Room") (uid . "same-uid")))

(defmacro cfw-macos-test-with-zone (zone &rest body)
  (declare (indent 1))
  `(let ((old-zone (getenv "TZ")))
     (unwind-protect
         (progn (set-time-zone-rule ,zone) ,@body)
       (set-time-zone-rule old-zone))))

(ert-deftest cfw-macos-disabled-and-other-platforms-do-no-work ()
  (cl-letf (((symbol-function 'cfw:macos--calendars)
             (lambda () (ert-fail "Unexpected system calendar access"))))
    (let ((cfw:macos-calendars nil)) (should-not (cfw:macos-create-sources)))
    (let ((system-type 'gnu/linux) (cfw:macos-calendars t))
      (should-not (cfw:macos-create-sources)))))

(ert-deftest cfw-macos-whitelist-and-org-exclusion ()
  (let ((cfw:macos-calendars '("日程" ("iCloud" . "携程") (:id "work") "Org"))
        (cfw:macos-excluded-calendars '("Org")))
    (should (equal '("personal" "travel" "work")
                   (mapcar (lambda (calendar) (alist-get 'id calendar))
                           (cfw:macos--selected-calendars cfw-macos-test-calendars))))))

(ert-deftest cfw-macos-all-still-excludes-org ()
  (let ((cfw:macos-calendars t) (cfw:macos-excluded-calendars '("Org")))
    (should (= 3 (length (cfw:macos--selected-calendars cfw-macos-test-calendars))))))

(ert-deftest cfw-macos-exclusions-can-be-disabled ()
  (let ((cfw:macos-calendars '("Org")) (cfw:macos-excluded-calendars nil))
    (should (equal "org-copy" (alist-get 'id (car (cfw:macos--selected-calendars cfw-macos-test-calendars)))))))

(ert-deftest cfw-macos-duplicate-titles-require-an-account ()
  (let* ((calendars '(((id . "a") (source . "iCloud") (title . "Holiday"))
                      ((id . "b") (source . "Subscriptions") (title . "Holiday"))))
         (cfw:macos-excluded-calendars nil)
         (cfw:macos-calendars '("Holiday")))
    (should-error (cfw:macos--selected-calendars calendars) :type 'user-error)
    (setq cfw:macos-calendars '(("iCloud" . "Holiday")))
    (should (equal "a" (alist-get 'id (car (cfw:macos--selected-calendars calendars)))))))

(ert-deftest cfw-macos-missing-calendar-is-reported ()
  (let ((cfw:macos-calendars '("Missing")))
    (should-error (cfw:macos--selected-calendars cfw-macos-test-calendars) :type 'user-error)))

(ert-deftest cfw-macos-overlapping-selectors-do-not-duplicate-sources ()
  (let ((cfw:macos-calendars '("日程" (:id "personal"))))
    (should (= 1 (length (cfw:macos--selected-calendars cfw-macos-test-calendars))))))

(ert-deftest cfw-macos-single-day-all-day-event-has-no-spurious-next-day ()
  (cfw-macos-test-with-zone "UTC"
    (let ((event (cfw:macos--convert-event
                  (cfw-macos-test-event "2026-10-16 00:00" "2026-10-17 00:00" t)
                  (car cfw-macos-test-calendars))))
      (should (equal '(10 16 2026) (cfw:event-start-date event)))
      (should-not (cfw:event-start-time event))
      (should-not (cfw:event-end-date event)))))

(ert-deftest cfw-macos-multi-day-all-day-end-is-inclusive ()
  (cfw-macos-test-with-zone "UTC"
    (let ((event (cfw:macos--convert-event
                  (cfw-macos-test-event "2026-10-16 00:00" "2026-10-19 00:00" t)
                  (car cfw-macos-test-calendars))))
      (should (equal '(10 18 2026) (cfw:event-end-date event))))))

(ert-deftest cfw-macos-timed-event-retains-time-and-context ()
  (cfw-macos-test-with-zone "UTC"
    (let ((event (cfw:macos--convert-event
                  (cfw-macos-test-event "2026-10-16 09:30" "2026-10-16 10:15")
                  (car cfw-macos-test-calendars))))
      (should (equal '(9 30) (cfw:event-start-time event)))
      (should (equal '(10 15) (cfw:event-end-time event)))
      (should-not (cfw:event-end-date event))
      (should (= 930 (get-text-property 0 'time-of-day (cfw:event-title event))))
      (should (equal "Room" (cfw:event-location event)))
      (should (string-match-p "iCloud / 日程" (cfw:event-description event))))))

(ert-deftest cfw-macos-overnight-and-midnight-boundaries ()
  (cfw-macos-test-with-zone "UTC"
    (let ((overnight (cfw:macos--convert-event
                      (cfw-macos-test-event "2026-10-16 23:30" "2026-10-17 01:00")
                      (car cfw-macos-test-calendars)))
          (midnight (cfw:macos--convert-event
                     (cfw-macos-test-event "2026-10-16 23:30" "2026-10-17 00:00")
                     (car cfw-macos-test-calendars))))
      (should (equal '(10 17 2026) (cfw:event-end-date overnight)))
      (should-not (cfw:event-end-date midnight))
      (should (string-match-p "2026-10-17 00:00" (cfw:event-description midnight))))))

(ert-deftest cfw-macos-query-end-handles-daylight-saving ()
  (cfw-macos-test-with-zone "America/New_York"
    (should (= (* 23 60 60)
               (- (float-time (cfw:macos--encode-date '(3 8 2026) t))
                  (float-time (cfw:macos--encode-date '(3 8 2026))))))))

(ert-deftest cfw-macos-sources-share-query-and-refresh-invalidates-cache ()
  (let ((system-type 'darwin) (cfw:macos-calendars t)
        (cfw:macos-excluded-calendars '("Org"))
        (cfw:macos--event-cache (make-hash-table :test #'equal))
        (calls 0) requests)
    (cl-letf (((symbol-function 'cfw:macos--calendars) (lambda () cfw-macos-test-calendars))
              ((symbol-function 'cfw:macos--call)
               (lambda (&rest arguments) (cl-incf calls) (push arguments requests)
                 '((version . 1) (events)))))
      (let ((sources (cfw:macos-create-sources)))
        (should (= 3 (length sources)))
        (should (equal "#0000ff" (cfw:source-color (car sources))))
        (dolist (source sources) (funcall (cfw:source-data source) '(10 1 2026) '(10 31 2026)))
        (should (= 1 calls))
        (should-not (member "org-copy" (car requests)))
        (funcall (cfw:source-update (car sources)))
        (funcall (cfw:source-data (car sources)) '(10 1 2026) '(10 31 2026))
        (should (= 2 calls))))))

(ert-deftest cfw-macos-cache-expires ()
  (let ((cfw:macos--event-cache (make-hash-table :test #'equal))
        (cfw:macos-cache-seconds 0) (calls 0))
    (cl-letf (((symbol-function 'cfw:macos--call)
               (lambda (&rest _) (cl-incf calls) '((version . 1) (events)))))
      (dotimes (_ 2) (cfw:macos--events '(10 1 2026) '(10 2 2026) '("personal")))
      (should (= 2 calls)))))

(ert-deftest cfw-macos-recurring-occurrences-are-not-deduplicated-by-uid ()
  (cfw-macos-test-with-zone "UTC"
    (let ((events (list (cfw-macos-test-event "2026-10-16 09:00" "2026-10-16 10:00")
                        (cfw-macos-test-event "2026-10-17 09:00" "2026-10-17 10:00"))))
      (cl-letf (((symbol-function 'cfw:macos--events) (lambda (&rest _) events)))
        (should (= 2 (length (cfw:macos--source-data
                              (car cfw-macos-test-calendars) '("personal")
                              '(10 1 2026) '(10 31 2026)))))))))

(ert-deftest cfw-macos-periods-use-real-calfw-source-format ()
  (cfw-macos-test-with-zone "UTC"
    (let ((events (list (cfw-macos-test-event "2026-10-16 00:00" "2026-10-19 00:00" t))))
      (cl-letf (((symbol-function 'cfw:macos--events) (lambda (&rest _) events)))
        (let* ((source (make-cfw:source :name "System"))
               (data (cfw:macos--source-data (car cfw-macos-test-calendars)
                                            '("personal") '(10 1 2026) '(10 31 2026)))
               (merged (cfw:contents-put-source data source))
               (period (cadr (car merged))))
          (should (equal '(10 16 2026) (car period)))
          (should (equal '(10 18 2026) (cadr period)))
          (should (cfw:event-p (nth 2 period))))))))

(ert-deftest cfw-macos-org-integration-renders-both-sources ()
  (let* ((file (make-temp-file "calfw-org-" nil ".org"))
         (org-agenda-files (list file))
         (cfw:org-calendar-default-view 'two-weeks)
         (cfw:display-calendar-holidays nil)
         (cfw:org-agenda-schedule-args '(:timestamp))
         (org-todo-keywords-for-agenda '("TODO" "DONE"))
         (today (calendar-current-date)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (format "* Local\n<%04d-%02d-%02d>\n" (nth 2 today) (car today) (cadr today))))
          (cl-letf (((symbol-function 'cfw:macos-create-sources)
                     (lambda ()
                       (list (make-cfw:source
                              :name "System" :color "blue"
                              :data (lambda (&rest _)
                                      (list (make-cfw:event :title "Remote" :start-date today))))))))
            (save-window-excursion
              (cfw:open-org-calendar)
              (with-current-buffer (window-buffer (selected-window))
                ;; Small windows collapse entries; the complete day must remain available.
                (let ((details (cfw:details-layout today (cfw:component-model (cfw:cp-get-component)))))
                  (should (string-match-p "Local" details))
                  (should (string-match-p "Remote" details)))))))
      (let ((buffer (get-file-buffer file))) (when buffer (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest cfw-macos-permission-failure-keeps-org-calendar-usable ()
  (let ((warnings nil) (seen-sources nil))
    (cl-letf (((symbol-function 'cfw:macos-create-sources) (lambda () (error "Calendar access denied")))
              ((symbol-function 'display-warning) (lambda (&rest args) (push args warnings)))
              ((symbol-function 'cfw:create-calendar-component-buffer)
               (lambda (&rest args)
                 (setq seen-sources (plist-get args :contents-sources)) 'component))
              ((symbol-function 'cfw:cp-get-buffer) (lambda (_) (get-buffer-create " *calfw mock*"))))
      (save-window-excursion (cfw:open-org-calendar))
      (should (= 1 (length seen-sources)))
      (should (equal "org-agenda" (cfw:source-name (car seen-sources))))
      (should warnings))
    (kill-buffer " *calfw mock*")))

(ert-deftest cfw-macos-helper-uses-its-own-app-and-private-output-files ()
  (let ((cfw:macos-helper-directory "/private/tmp/calfw-test-helper/"))
    (cl-letf (((symbol-function 'cfw:macos--ensure-helper) (lambda () "unused"))
              ((symbol-function 'process-file)
               (lambda (program _in _out _display &rest arguments)
                 (should (equal program "/usr/bin/open"))
                 (should (member "-g" arguments))
                 (should-not (member "-W" arguments))
                 (should (equal (cadr (member "-a" arguments))
                                "/private/tmp/calfw-test-helper/CalfwCalendar.app"))
                 (should (equal (cdr (member "--args" arguments)) '("status")))
                 (let ((output (cadr (member "--stdout" arguments))))
                   (should (= (logand (file-modes output) #o777) #o600))
                   (with-temp-file output
                     (insert "{\"version\":1,\"authorization\":\"not-determined\"}\n")))
                 0)))
      (should (equal "not-determined"
                     (alist-get 'authorization (cfw:macos--call "status")))))))

(ert-deftest cfw-macos-helper-denial-is-not-mistaken-for-an-empty-calendar ()
  (cl-letf (((symbol-function 'cfw:macos--ensure-helper) (lambda () "unused"))
            ((symbol-function 'process-file)
             (lambda (_program _in _out _display &rest arguments)
               (with-temp-file (cadr (member "--stderr" arguments))
                 (insert "Calendar read access is denied. Run cfw:macos-request-access.\n"))
               ;; LaunchServices succeeds even when the helper exits with an error.
               0)))
    (should-error (cfw:macos--call "calendars") :type 'error)))

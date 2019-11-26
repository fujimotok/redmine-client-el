(defvar redmine-client-redmine-url "https://my.redmine.jp/demo")
(defvar redmine-client-issues-offset nil)
(defvar redmine-client-issues-limit nil)
(defvar redmine-client-issues-total nil)

;; to load parse-iso8601-time-string
(parse-time-string "")

(defun url-http-get (url &optional args method header)
  ""
  (let ((response-string nil)
        (coding-system-string nil)
        (url-request-method (or method "GET"))
        (url-extensions-header (or header nil))
        (request-url url))
    (if args
        (setq request-url
              (concat url "?" 
                      (mapconcat (lambda (param)
                                   (concat (url-hexify-string (format "%s" (car param)))
                                           "="
                                           (url-hexify-string (format "%s" (cdr param)))))
                                 args "&"))
              ))
    (message "%s" request-url)
    (with-current-buffer (url-retrieve-synchronously request-url t nil 3)
      (goto-char (point-min))
      (re-search-forward "charset=")
      (setq coding-system-string
            (buffer-substring-no-properties (point) (point-at-eol)))
      (re-search-forward "\n\n")
      (setq response-string
            (buffer-substring-no-properties (point) (point-max))))
    (decode-coding-string response-string
                          (coding-system-from-name coding-system-string))))

;; (redmine-client-get-time 0 5 '((from . "2019-11-20") (to . "2019-11-22")))

(defun redmine-client-get-time (&optional offset limit filter)
  "Get time entries"
  (interactive)
  (let ((issue-alist nil)
        (buffer (get-buffer-create "*issues*")))
    (setq offset (or offset 0))
    (setq limit  (or limit 25))
    (setq issues-alist
          (json-read-from-string
           (url-http-get (format "%s/time_entries.json"
                                 redmine-client-redmine-url)
                         (list (cons "offset" offset)
                               (cons "limit"  limit)))))
    (setq redmine-client-issues-offset (cdr (assoc 'offset issues-alist)))
    (setq redmine-client-issues-limit  (cdr (assoc 'limit  issues-alist)))
    (setq redmine-client-issues-total  (cdr (assoc 'total_count issues-alist)))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert (format "host:  %s\n" redmine-client-redmine-url))
    (insert (format "count: %d\n\n" redmine-client-issues-total))
    (insert (format "#id %-50s assigned_to\n" "subject"))
    (insert "---------------------------------------------------------------------\n")
    (mapcar (lambda (x)
              (insert (format "#%d %s %s %-50s\n"
                              ;; (cdr (assoc 'id x))
                              (cdr (assoc 'id (cdr (assoc 'issue x))))
                              (cdr (assoc 'spent_on x))
                              (cdr (assoc 'hours x))
                              (truncate-string-to-width (cdr (assoc 'comments x)) 50)
                              ;; (or (cdr (assoc 'name (assoc 'assigned_to x))) "-")
                              )))
            (cdr (assoc 'time_entries issues-alist)))
    ;; (insert (format "\n     < back     %d / %d page [%d items]     forward >"
    ;;                 (ceiling (1+ redmine-client-issues-offset) redmine-client-issues-limit)
    ;;                 (ceiling redmine-client-issues-total redmine-client-issues-limit)
    ;;                 redmine-client-issues-limit))
    ;; (redmine-client-issues-buttonize)
    (read-only-mode 1)
    ;;(use-local-map redmine-client-mode-map)
    (pop-to-buffer buffer))
  )

(defun redmine-client-get-issues (&optional offset limit filter)
  "Get issues"
  (interactive)
  (let ((issue-alist nil)
        (buffer (get-buffer-create "*issues*")))
    (setq offset (or offset 0))
    (setq limit  (or limit 25))
    (setq issues-alist
          (json-read-from-string
           (url-http-get (format "%s/issues.json"
                                 redmine-client-redmine-url)
                         (list (cons "offset" offset)
                               (cons "limit"  limit)))))
    (setq redmine-client-issues-offset (cdr (assoc 'offset issues-alist)))
    (setq redmine-client-issues-limit  (cdr (assoc 'limit  issues-alist)))
    (setq redmine-client-issues-total  (cdr (assoc 'total_count issues-alist)))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert (format "host:  %s\n" redmine-client-redmine-url))
    (insert (format "count: %d\n\n" redmine-client-issues-total))
    (insert (format "#id    %-50s assigned_to\n" "subject"))
    (insert "---------------------------------------------------------------------\n")
    (mapcar (lambda (x)
              (insert (format "#%d %-50s %s\n"
                              (cdr (assoc 'id x))
                              (truncate-string-to-width (cdr (assoc 'subject x)) 50)
                              (or (cdr (assoc 'name (assoc 'assigned_to x))) "-") )))
            (cdr (assoc 'issues issues-alist)))
    (insert (format "\n     < back     %d / %d page [%d items]     forward >"
                    (ceiling (1+ redmine-client-issues-offset) redmine-client-issues-limit)
                    (ceiling redmine-client-issues-total redmine-client-issues-limit)
                    redmine-client-issues-limit))
    (redmine-client-issues-buttonize)
    (read-only-mode 1)
    (use-local-map redmine-client-mode-map)
    (pop-to-buffer buffer))
  )

(defun redmine-client-get-issue-id (&optional button)
  "get issue id in current line"
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "#[0-9]+"))
        (message "Can't find issue item.")
      (re-search-forward "#\\([0-9]+\\)" nil t)
      (redmine-client-get-issue (string-to-number (match-string 1)))
      )
    )
  )

(defun redmine-client-issues-buttonize ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "< back" nil t)
    (make-button (match-beginning 0) (match-end 0) :type 'redmine-client-back-button)
    (re-search-forward "\[[0-9]+ items\]" nil t)
    (make-button (match-beginning 0) (match-end 0) :type 'redmine-client-limit-button)
    (re-search-forward "forward >" nil t)
    (make-button (match-beginning 0) (match-end 0) :type 'redmine-client-forward-button)
    (goto-char (point-min))
    (while (re-search-forward "#[0-9]+" nil t)
      (make-button (match-beginning 0) (match-end 0) :type 'redmine-client-issue-button)
      )
    ))

(define-button-type 'redmine-client-back-button
  'follow-link t
  'action #'redmine-client-issues-back)

(define-button-type 'redmine-client-forward-button
  'follow-link t
  'action #'redmine-client-issues-forward)

(define-button-type 'redmine-client-limit-button
  'follow-link t
  'action #'redmine-client-issues-set-limit)

(define-button-type 'redmine-client-issue-button
  'follow-link t
  'action #'redmine-client-get-issue-id)

(defun redmine-client-issues-back (&optional button)
  (interactive)
  (redmine-client-get-issues
   (if (< 0 (- redmine-client-issues-offset redmine-client-issues-limit))
       (- redmine-client-issues-offset redmine-client-issues-limit) 0)
   redmine-client-issues-limit)
  )

(defun redmine-client-issues-forward (&optional button)
  (interactive)
  (redmine-client-get-issues
   (if (< (+ redmine-client-issues-offset redmine-client-issues-limit)
          redmine-client-issues-total)
       (+ redmine-client-issues-offset redmine-client-issues-limit)
     redmine-client-issues-total)
   redmine-client-issues-limit)
  )

(defun redmine-client-issues-set-limit (&optional button)
  (let (limit
        limit-string)
    (setq limit redmine-client-issues-limit)
    (setq limit-string (read-from-minibuffer "Set page items count:"
                                             (format "%d" limit)))
    (setq limit (string-to-number limit-string))
    (if (not (< 0 limit))
        (message "invalid value %s" limit-string)
      (setq redmine-client-issues-limit limit-string)
      (redmine-client-get-issues redmine-client-issues-offset
                                 redmine-client-issues-limit)
      )
    )
  )

(defvar redmine-client-mode-map (make-sparse-keymap) "redmine-client-mode keymap")
(define-key redmine-client-mode-map "<" #'redmine-client-issues-back)
(define-key redmine-client-mode-map ">" #'redmine-client-issues-forward)

(defun redmine-client-get-issue (issue-id)
  "Get varbse issue"
  (let ((issue-alist nil)
        (option '(("include" . "children,attachments,relations,changesets,journals,watchers")))
        (buffer (get-buffer-create (format "*issue [%d]*" issue-id))))
    (setq issue-alist
          (cdr (assoc 'issue
                      (json-read-from-string
                       (url-http-get (format "%s/issues/%d.json"
                                             redmine-client-redmine-url issue-id)
                                     option)))))
    (set-buffer buffer)
    (read-only-mode -1)
    (erase-buffer)
    (insert (format "#%d %s" (cdr (assoc 'id issue-alist)) (cdr (assoc 'subject issue-alist))))
    (add-face-text-property (point-at-bol) (point) '(:height 1.5))
    (insert "\n")
    (insert "===========================================================\n")
    (insert (format "update: %s  "
                    (format-time-string "%F %R"
                                        (parse-iso8601-time-string
                         (cdr (assoc 'updated_on issue-alist))))))
    (insert (format "create: %s\n"
                    (format-time-string "%F %R"
                                        (parse-iso8601-time-string
                                         (cdr (assoc 'created_on issue-alist))))))
    (insert (format "start:  %s        " (cdr (assoc 'start_date issue-alist))))
    (insert (format "deadline: %s\n"     (cdr (assoc 'due_date issue-alist))))
    (insert "-----------------------------------------------------------\n")
    (insert (format "done_ratio: %3s%% [" (cdr (assoc 'done_ratio issue-alist))))
    (dotimes (i (floor (cdr (assoc 'done_ratio issue-alist)) 10)) (insert "="))
    (dotimes (i (- 10 (floor (cdr (assoc 'done_ratio issue-alist)) 10))) (insert "-"))
    (insert "]\n")
    (insert (format "spent_time:  %sH  "
                    (or (cdr (assoc 'spent_hours issue-alist)) "-")))
    (insert (format "estimated_time: %sH\n"
                    (or (cdr (assoc 'estimated_hours issue-alist)) "-")))
    (insert (format "total_spent: %sH  "
                    (or (cdr (assoc 'total_spent_hours issue-alist)) "-")))
    (insert (format "total_estimated: %sH\n"
                    (or (cdr (assoc 'total_estimated_hours issue-alist)) "-")))
    (insert "-----------------------------------------------------------\n")
    (insert (format "project:  %s\n"
                    (or (cdr (assoc 'name (assoc 'project issue-alist))) "-")))
    (insert (format "tracker:  %s\n"
                    (or (cdr (assoc 'name (assoc 'tracker issue-alist))) "-")))
    (insert (format "category: %s\n"
                    (or (cdr (assoc 'name (assoc 'category issue-alist))) "-")))
    (insert (format "version:  %s\n"
                    (or (cdr (assoc 'name (assoc 'fixed_version issue-alist))) "-")))
    (insert (format "priority: %s\n"
                    (or (cdr (assoc 'name (assoc 'priority issue-alist)))) "-"))
    (insert "-----------------------------------------------------------\n")
    (insert (format "author:   %s\n"
                    (or (cdr (assoc 'name (assoc 'author issue-alist))) "-")))
    (insert (format "assigned: %s\n"
                    (or (cdr (assoc 'name (assoc 'assigned_to issue-alist))) "-")))
    (insert (format "parent:   %s\n"
                    (or (cdr (assoc 'id (assoc 'parent issue-alist))) "-")))
    (insert (format "children: %s\n" (if (cdr (assoc 'children issue-alist)) "" "-")))
    (mapcar (lambda (x)
              (insert (format "    #%d %s\n" (cdr (assoc 'id x)) (cdr (assoc 'subject x)))))
            (cdr (assoc 'children issue-alist)))
    (insert "-----------------------------------------------------------\n")
    (insert (format "description:\n %s\n"
                    (cdr (assoc 'description issue-alist))))
    (insert "===========================================================\n")
    (mapcar (lambda (x)
              (insert (format "date: %s\n"
                              (format-time-string "%F %R"
                                                  (parse-iso8601-time-string
                                                   (cdr (assoc 'created_on x))))))
              (insert (format "author: %s\n" (cdr (assoc 'name (assoc 'user x)))))
              (mapcar (lambda (y)
                        (insert (format "[%s: %s -> %s]\n"
                                        (cdr (assoc 'name y))
                                        (or (cdr (assoc 'old_value y)) "-")
                                        (or (cdr (assoc 'new_value y)) "-"))))
                      (cdr (assoc 'details x)))
              (if (cdr (assoc 'notes x)) (insert (format "%s\n" (cdr (assoc 'notes x)))))
              (insert "-----------------------------------------------------------\n"))
            (cdr (assoc 'journals issue-alist)))
    (read-only-mode 1)
    (pop-to-buffer buffer)
    )
  )

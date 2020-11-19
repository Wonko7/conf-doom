;;; ../conf/doom/yolobolo.el -*- lexical-binding: t; -*-

(require 'org-protocol)
(require 'ol) ;; for org-link-decode

(defun make-tab-entry (title url)
  (let ((stars     (make-string 2 ?*))
        (timestamp (format-time-string "[%F %a %H:%M]\n")))
    (concat stars " [[" url "][" title "]]\n"
            timestamp)))

;;;; Functions
(defun yolobolo-save (tab-list)
  (interactive "P")
  (message "yep")
  (let* ((session (cadr tab-list))
         (tabs (cl-loop for (a title c url) in (seq-partition (cddr tab-list) 4)
                        collect (make-tab-entry title url)))
         (tabs (cons (concat "* " session "   :browsing-session:\n") tabs))
         (tabs (apply #'concat tabs)))
    (print session)
    (print tabs)
    (print session)
    nil))

;; so the filtering of data should happen in emacs really. ff dumps max data.
;; so. column viewer might be helpful here.
;; can I filter by domain?
;; start thinking about what we can do with org-ql next.
(push '("lol-yolobolo" :protocol "yolobolo" :function yolobolo-save)
      org-protocol-protocol-alist)

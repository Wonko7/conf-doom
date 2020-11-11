;;; ../conf/doom/yolobolo.el -*- lexical-binding: t; -*-

(require 'org-protocol)
(require 'ol) ;; for org-link-decode

;;;; Functions
(defun yolobolo-save (info)
  (interactive "P")
  (print "yes this is it")
  (pp info)
  (pp info)
  (print "car")
  (pp (car info))
  (print "cdr")
  (pp (cdr info))
  (print "cadr")
  (pp (cadr info))
  (let ((thing (org-protocol-split-data (cdr info) t)))
    (print "decoded" thing))
  nil)

;; so the filtering of data should happen in emacs really. ff dumps max data.
;; so. column viewer might be helpful here.
;; can I filter by domain?
;; start thinking about what we can do with org-ql next.
(push '("lol-yolobolo" :protocol "yolobolo" :function yolobolo-save)
      org-protocol-protocol-alist)

;;; ../conf/doom/org-conf.el -*- lexical-binding: t; -*-


(setq org-directory "/data/org/")
(setq org-roam-directory (concat org-directory "here-be-dragons/"))
(setq org-journal-dir (concat org-directory "the-road-so-far/"))
(setq org-agenda-files (cons org-directory (mapcar
                                            (lambda (d)
                                              (concat org-directory d))
                                            '("people/" "wip/" "work/" "the-road-so-far/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org packages:


;; (use-package! org-roam
;;   :hook (after-init . org-roam-mode))
(use-package! org-roam
  :after org
  :hook (after-init . org-roam-mode))

;; (use-package! org-roam-protocol
;;   :after org-protocol)
;;
;; (map! :map org-agenda-mode-map
;;       :nvm "j" #'org-agenda-next-line
;;       :nvm "k" #'org-agenda-previous-line)
;; (map! :map evil-org-agenda-mode-map
;;       :nvm "j" #'org-agenda-next-line
;;       :nvm "k" #'org-agenda-previous-line)

;; (after! org-agenda
;;  (map! :map evil-org-agenda-mode-map
;;       :nvm "j" #'org-agenda-next-line
;;       :nvm "k" #'org-agenda-previous-line)
;;  (map! :map org-agenda-mode-map
;;       :nvm "j" #'org-agenda-next-line
;;       :nvm "k" #'org-agenda-previous-line))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-compact-blocks t
        ;org-agenda-start-with-follow-mode t
        org-super-agenda-header-separator "\n")
  :config
  (org-super-agenda-mode))

(use-package! org-crypt
  :config
  (org-crypt-use-before-save-magic))

;; FIXME/review show past:
;;  https://github.com/bastibe/org-journal/issues/260
(use-package! org-journal)

;; org journal template:
(set-file-template! "/20[-[:digit:]]+_[[:alpha:]]+\\.org$"
  ;; :trigger "__"
  :mode 'org-journal-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org conf:

(after! org
  (setq
   org-plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar"
   org-extend-today-until 3
   org-todo-keywords
        '((sequence
           "NEXT(n/!)"  ; A task that recuring
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "GOGO(g/!)"  ; A task that is in progress
           "WAIT(w/!)"  ; Something external is holding up this task
           "HOLD(h/!)"  ; This task is paused/on hold because of me
           "ADD(a)"  ; feature
           "FIX(f)"  ; feature
           "BUG(b)"  ; feature
           "|"
           "DONE(d/!)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](G)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("GOGO" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-crypt, habbit, & related:

;; make decrypting a bit more magical: decrypt after save
;; use this to toggle feature: (remove-hook 'after-save-hook 'restore-point t)
(defun save-point ()
  (setq-local my/yolo (point)))

(defun restore-point ()
  (org-decrypt-entries)
  (goto-char my/yolo))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-hook 'after-save-hook 'restore-point nil t)
   (add-hook 'before-save-hook 'save-point (- 42) t)))

(after! org-crypt
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-disable-auto-save "encrypt"
        org-crypt-key "william@underage.wang"))

(after! org-habit
  (setq org-habit-graph-column 60))

(after! org
  (setq org-agenda-start-on-weekday 1
        calendar-week-start-day 1
        org-log-into-drawer t
        org-auto-align-tags t
        org-tags-column 72))
(setq cfw:org-agenda-schedule-args '(:timestamp))

;; calendar
(defun fuck-me/init-cal ()
  (setq org-gcal-client-id (auth-source-pass-get "id" "tokens/caldav/calendar.google.com")
        org-gcal-client-secret (auth-source-pass-get 'secret "tokens/caldav/calendar.google.com")
        org-gcal-fetch-file-alist `((,(auth-source-pass-get "work" "tokens/caldav/calendar.google.com") .  ,(concat org-directory "work/wobbly.org"))
                                    (,(auth-source-pass-get "perso" "tokens/caldav/calendar.google.com") .  ,(concat org-directory "the-road-so-far/wibbly.org")))))

(use-package! org-gcal
  ;:ensure t
  :after password-store
  :config
  (fuck-me/init-cal))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; appt

(require 'appt)

(setq appt-message-warning-time 10) ; Show notification 5 minutes before event
(setq appt-display-interval 5) ; Disable multiple reminders
;; (setq appt-display-mode-line nil)

(add-hook 'org-agenda-mode-hook #'org-agenda-to-appt)
(run-at-time "12:01am" (* 24 3600) 'org-agenda-to-appt)

(add-hook 'after-save-hook
          (lambda ()
             (when (string-prefix-p org-directory buffer-file-name)
               (org-agenda-to-appt))))

; Display appointments as a window manager notification
(setq appt-disp-window-function #'my/appt-notif)
(setq appt-delete-window-function (lambda () t))

(setq my-appt-notification-app (concat (getenv "HOME") "/bin/appt-notification"))

(defun my/appt-notif (min-to-appt new-time msg)
  (if (atom min-to-appt)
      (call-process "notif" nil 0 nil "send" (concat "RDV: " min-to-appt) msg)
  (dolist (i (number-sequence 0 (1- (length min-to-appt))))
    (call-process "notif" nil 0 nil "send" (concat "RDV: " (nth i min-to-appt)) (nth i msg)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; capture

(defun my/insert-inactive-timestamp ()
  (interactive)
  (insert (format-time-string "[%F %a %H:%M]")))

(defun my/journal-open-today (&optional other-window)
  (let ((fpath (expand-file-name (format-time-string "%F_%A.org") org-journal-dir)))
    (if other-window
        (find-file-other-window fpath)
      (find-file fpath))
    (org-decrypt-entries) ;; decrypt org entries before trying to add stuff in them, olp can't work on opaque gpg.
    (when (<= (point-max) 300) ;; FIXME yeahhhhhhh there's probably a better test.
      (org-journal--carryover))
    fpath))

(defun my/log-entry (olp-path)
  (let ((m (org-find-olp (cons (org-capture-expand-file (my/journal-open-today))
                               (cons (format-time-string "%F %A")
                                     olp-path)))))
    (goto-char m)))

(defun my/work-monthly-log-file (name)
  (let ((path (concat "work/" name (format-time-string "_%+4Y-%m-00.org"))))
    path))

(defun my/monthly-log-file (name)
  (let ((path (concat "the-road-so-far/" name (format-time-string "_%+4Y-%m-00.org"))))
    path))

(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform)))

(defun fuck-me/init-capture ()
  (setq org-capture-projects-file "dev"
        ;; add project stuff.
        org-capture-templates
        `(("d" "ssdd" entry (function (lambda ()
                                        (my/log-entry '("ssdd"))))

           "* TODO %?\n%t" :prepend t)
          ("s" "ssdd" entry (function (lambda () ;; I appear to prefer s
                                        (my/log-entry '("ssdd"))))
           "* TODO %?\n%t")

          ;; ("w" "work quick" entry (function (lambda ()
          ;;                                   (my/log-entry '("log" "work"))) )
          ;;  "* %?\n"
          ;;  :no-save t
          ;;  :clock-in t)
          ("W" "log work" entry (function (lambda ()
                                                  (my/log-entry '("log" "work"))) )
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)

          ("r" "RDV" entry
           (file+olp ,(expand-file-name "wibbly.org" org-journal-dir) "wibbly" "inbox")
           ,(concat "* %?\n :PROPERTIES:\n :calendar-id: " (password-store-get "web/google/caldav/perso") "\n :END:\n:org-gcal:\n%^T\n:END:\n\n")
           :jump-to-captured t)

          ("t" "todo to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* TODO %?\n%U\n")
          ("n" "note to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %?\n%U\n")
          ("N" "note to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %?\n%U\n")
          ("g" "groceries" item
           (file+olp "lol.org" "lol" "TODO shopping" "TODO groceries")
           "- %?\n")

          ;; besport
          ("b" "BeSport")

          ("br" "RDV" entry
           (file+olp ,(expand-file-name "work/wobbly.org" org-directory) "wobbly" "inbox")
           ,(concat "* %?\n :PROPERTIES:\n :calendar-id: " (password-store-get "web/google/caldav/work") "\n :END:\n:org-gcal:\n%^T\n:END:\n\n")
           :jump-to-captured t)

          ;; ("ba" "Agenda/RDV" entry
          ;;  (file+olp "work/besport.org" "besport" "agenda" "inbox")
          ;;  "* 🐫 %?\n%^t\n")
          ("jf" "witness the fitness" entry (file+olp+datetree )
           "* %?\n%U\n"
           :jump-to-captured t)
          ("bn" "Notes" entry
           (file+olp+datetree ,(my/work-monthly-log-file "blackbox"))
           "* %?\n%U\n"
           :jump-to-captured t)
          ("bt" "todo" entry
           (file+olp "work/besport.org" "besport" "inbox")
           "* TODO %? :bs:\n%U\n")
          ("bb" "todo" entry
           (file+olp "work/besport.org" "besport" "inbox")
           "* TODO %?\n%U\n")

          ("f" "Templates for notes from files")
          ("ft" "todo from file" entry
           (file+olp "lol.org" "lol" "inbox")
           "* TODO %?\n%a")
          ("fn" "note from file" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %U %?\n%a")

          ("j" "journal")
          ("jf" "witness the fitness" entry (file+olp+datetree ,(my/monthly-log-file "witness-the-fitness"))
           "* %?\n%U\n"
           :jump-to-captured t)
          ("jw" "witness the fitness" entry (file+olp+datetree ,(my/monthly-log-file "witness-the-fitness"))
           "* %?\n%U\n"
           :jump-to-captured t)

          ("ji" "innerspace" entry (file+olp+datetree ,(my/monthly-log-file "innerspace"))
           "* %?\n%U\n"
           :jump-to-captured t)


          ("c" "log")
          ("cf" "witness the fitness" entry (function (lambda ()
                                                        (my/log-entry '("log" "witness the fitness"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)


          ("ci" "innerspace" entry (function (lambda ()
                                               (my/log-entry '("log" "innerspace"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)


          ("cw" "work" entry (function (lambda ()
                                         (my/log-entry '("log" "work"))) )
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)

          ("cc" "chores" entry (function (lambda ()
                                           (my/log-entry '("log" "chores"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)

          ("cb" "bs" entry (function (lambda ()
                                       (my/log-entry '("log" "bs"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)

          ("Qp" "Protocol" entry
           (file+olp "browsing/inbox.org" "browsing" "inbox")
           "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
           :immediate-finish t)
          ("QL" "Protocol Link direct" entry
           (file+olp "browsing/inbox.org" "browsing" "inbox")
           "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%U"
           :immediate-finish t)

          ;;("j" "Journal" entry
          ;; (file+olp+datetree +org-capture-journal-file)
          ;; "* %U %?\n%i\n%a" :prepend t)

          ;; TODO these look nice, look into this:
          ;;

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t))))

(after! org-capture
  (fuck-me/init-capture))

(after! org-journal
  (setq org-journal-enable-agenda-integration t
        org-journal-enable-cache t
        org-journal-time-format ""
        org-journal-time-prefix "*** "
        org-journal-hide-entries-p nil
        org-journal-carryover-items "next|TODO=\"PROJ\"|TODO=\"TODO\"|TODO=\"GOGO\"|TODO=\"[ ]\"|TODO=\"\\[ \\]\"|TODO=\"\\[\\\\]\"" ;; checkboxes do not work FIXME
        org-journal-file-format "%F_%A.org"
        org-journal-date-format "%F %A"))

(after! org-agenda
  (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'"
        org-agenda-prefix-format (quote
                                  ((agenda . "%-21c%?-12t% s")
                                   (timeline . "% s")
                                   (todo . "%-21c")
                                   (tags . "%-12c")
                                   (search . "%-12c")))
        org-agenda-deadline-leaders (quote ("!D!: " "D%2d: " ""))
        org-agenda-scheduled-leaders (quote ("" "S%3d: "))
        ;; fixes fucky binding on jk on an agenda header:
        ;; https://github.com/alphapapa/org-super-agenda/issues/50
        org-super-agenda-header-map (make-sparse-keymap)

        ;; (setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
        ;;       org-agenda-skip-scheduled-if-done t
        ;;       org-agenda-skip-deadline-if-done t
        ;;       org-agenda-include-deadlines t
        ;;       org-agenda-block-separator nil
        ;;       org-agenda-compact-blocks t
        ;;       org-agenda-start-with-log-mode t)
        ;;

        ;;       org-agenda-start-with-log-mode t)
        org-agenda-start-with-log-mode t
        org-habit-show-habits nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t

        org-agenda-custom-commands '(("c" "Simple agenda view"
                                      ((agenda "")
                                       (alltodo "" )))
                                     ("z" "Super zaen view"
                                      ((agenda "" )
                                       (alltodo "=" ((org-agenda-overriding-header "")
                                                     (org-super-agenda-groups
                                                      '((:name "ssdd"
                                                         :tag ("tt" "lol" "yolo" "ssdd")
                                                         :order 1)
                                                        (:name "BS"
                                                         :tag ("bs" "fm" "fuckme")
                                                         :order 1)
                                                        (:name "fun maximization"
                                                         :tag ("fun")
                                                         :order 2)
                                                        (:name "WWSCD"
                                                         :tag ("wwscd")
                                                         :order 3)
                                                        (:name "innerspace"
                                                         :tag ("is" "h" "habit" "focus")
                                                         :order 4)
                                                        (:name "review"
                                                         :tag ("review" "r")
                                                         :order 4)
                                                        (:name "next steps"
                                                         :tag "next"
                                                         :order 6)
                                                        (:name "Projects"
                                                         :todo "PROJ"
                                                         :order 7)
                                                        (:name "don't be a cunt"
                                                         :tag "dbac"
                                                         :order 8)
                                                        ;; (:name "repeat after me"
                                                        ;;  :order 9
                                                        ;;  :habit t
                                                        ;;  )
                                                        (:name ".*"
                                                         :order 10
                                                         :anything t
                                                         )
                                                        )))))))))

;; org advice newline bug:
;; https://github.com/hlissner/doom-emacs/issues/3172
;;(remove-hook! org-mode-hook #'electric-indent-mode)
;; (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))
;; (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(defun my/cut-entry ()
  (interactive)
  (save-excursion
    (progn 
      (org-capture-goto-last-stored)
      (goto-char (- (point) 1))
      (org-cut-subtree)))
  ;; ()
  ;;(org-paste-subtree)
  (insert  "\n")
  (insert (current-kill 0))
  )


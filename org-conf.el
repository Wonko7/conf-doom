;;; ../conf/doom/org-conf.el -*- lexical-binding: t; -*-

;; (setq org-element-use-cache nil)
;; "config.el"

(setq org-directory "/data/org/")
(setq org-roam-directory (concat org-directory "here-be-dragons/"))
(setq org-journal-dir (concat org-directory "the-road-so-far/"))
(setq org-agenda-files (cons org-directory (mapcar
                                            (lambda (d)
                                              (concat org-directory d))
                                            '("wip/" "work/" ;; "the-road-so-far/"
                                              "here-be-dragons/"
                                              "here-be-dragons/daily/"
                                              ))))

(after! elfeed
  ;(setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-search-filter "+unread +tf"))
(setq rmh-elfeed-org-files (list (concat org-directory "notes/rss/root.org")))
(map! :map elfeed-search-mode-map
      :nvm "RET" #'elfeed-search-show-entry)

(setq emacsql-sqlite-executable (executable-find "emacsql-sqlite"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org packages:


(use-package! org-roam
  :hook (after-init . org-roam-setup))

;; (use-package! org-roam-timestamps
;;   :after org-roam
;;   :config (org-roam-timestamps-mode))

;; (use-package! org-roam-ui
;;   :after org
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

; Workaround an upstream issue with evil, as described in https://github.com/syl20bnr/spacemacs/issues/14137
      ;; (defadvice org-roam-node-insert (around append-if-in-evil-normal-mode activate compile)
      ;;   "If in evil normal mode and cursor is on a whitespace character, then go into
      ;;    append mode first before inserting the link. This is to put the link after the
      ;;    space rather than before."
      ;;   (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
      ;;                                     (not (bound-and-true-p evil-insert-state-minor-mode))
      ;;                                     (looking-at "[[:blank:]]"))))
      ;;     (if (not is-in-evil-normal-mode)
      ;;         ad-do-it
      ;;       (evil-append 0)
      ;;       ad-do-it
      ;;       (evil-normal-state))))

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
;; (use-package! org-journal)

;; org journal template:
;; (set-file-template! "/20[-[:digit:]]+_[[:alpha:]]+\\.org$"
;;   ;; :trigger "__"
;;   :mode 'org-journal-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org conf:


(after! org
  (setq
   org-plantuml-jar-path (shell-command-to-string "cat `which plantuml` 2>/dev/null  | 2>/dev/null sed -nre 's/.* ([^ ]+\.jar).*/\\1/p' | tr -d '\n'")
   org-extend-today-until 3
   org-startup-folded 'content
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

;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;    (add-hook 'after-save-hook 'restore-point nil t)
;;    (add-hook 'before-save-hook 'save-point (- 42) t)))

(setq epa-file-encrypt-to '("william@underage.wang"))
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
        org-tags-column 72
        org-edit-timestamp-down-means-later t))
(setq cfw:org-agenda-schedule-args '(:timestamp))

;; calendar
(defun fuck-me/init-cal ()
  (setq org-gcal-client-id (+pass-get-field "tokens/caldav/calendar.google.com" "id")
        org-gcal-client-secret (+pass-get-secret  "tokens/caldav/calendar.google.com")
        org-gcal-fetch-file-alist `((,(+pass-get-field "tokens/caldav/calendar.google.com" "work") .  ,(concat org-directory "work/wobbly.org"))
                                    ;; (,(+pass-get-field "tokens/caldav/calendar.google.com" "perso") .  ,(concat org-directory "the-road-so-far/wibbly.org"))
                                    )))

(use-package! org-gcal
  ;:ensure t
  :after password-store
  :config
  (fuck-me/init-cal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; align

(defun my/org-mode-realign-all-tags ()
  (interactive)
  "Code to realign tags, stolen from org.el"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp-bol nil t)
      (org-set-tags (org-get-tags nil t))
      (end-of-line))))

(add-hook 'before-save-hook (lambda ()
                              (when (eq major-mode 'org-mode)
                                (my/org-mode-realign-all-tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; appt

(require 'appt)

(setq appt-message-warning-time 10) ; Show notification 5 minutes before event
(setq appt-display-interval 5) ; Disable multiple reminders
;; (setq appt-display-mode-line nil)

;; (add-hook 'org-agenda-mode-hook #'org-agenda-to-appt)
(run-at-time "12:01am" (* 24 3600) 'org-agenda-to-appt)

;; (add-hook 'after-save-hook
;;           (lambda ()
;;              (when (string-prefix-p org-directory buffer-file-name)
;;                (org-agenda-to-appt))))

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

(defvar my/daily-header "#+title: %<%Y-%m-%d>\n#+category: %<%Y-%m-%d>")
(defvar my/daily-file "%<%Y-%m-%d>.org")
(defun my/make-daily-capture (key desc entry jump)
  (list key desc 'entry entry
        :if-new (list 'file+head my/daily-file my/daily-header)
        :jump-to-captured jump))

(defun fuck-me/init-capture ()
  (setq org-capture-projects-file "dev"
        ;; add project stuff.
        org-capture-templates
        `(("d" "ssdd" entry (file+olp "lol.org" "lol" "ssdd")
           "* TODO %?\n%U" :prepend t)
          ("s" "ssdd" entry (file+olp "lol.org" "lol" "ssdd")
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
           ,(concat "* %?\n :PROPERTIES:\n :calendar-id: " (+pass-get-field "tokens/caldav/calendar.google.com" "perso") "\n :END:\n:org-gcal:\n%^T\n:END:\n\n")
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
           ,(concat "* %?\n :PROPERTIES:\n :calendar-id: " (+pass-get-field "tokens/caldav/calendar.google.com" "work") "\n :END:\n:org-gcal:\n%^T\n:END:\n\n")
           :jump-to-captured t)

          ;; ("ba" "Agenda/RDV" entry
          ;;  (file+olp "work/besport.org" "besport" "agenda" "inbox")
          ;;  "* 🐫 %?\n%^t\n")
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
           "* bouldering @%?\n%U\n** with :innerspace:\n** topped\n** projects\n** [[roam:injuries]]\n"
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
           (file+olp "here-be-dragons/20210915144652-browsing_inbox.org" "browsing" "inbox")
           "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
           :immediate-finish t)
          ("QL" "Protocol Link direct" entry
           (file+olp "here-be-dragons/20210915144652-browsing_inbox.org" "browsing" "inbox")
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
          ("pt" "Project-local todo" entry ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry ; {project-root}/changelog.org
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
           :prepend t))

        org-roam-dailies-capture-templates
        `(,(my/make-daily-capture "n" "note" "* %?\n%U\n" t)
          ("d" "ssdd top" entry "* [ ] %?"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🖖 ssdd"))
           :prepend t)
          ("s" "ssdd bottom" entry "* [ ] %?"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🖖 ssdd")))
          ("g" "go" plain "%?" :jump-to-captured t :if-new (file ,my/daily-file))

          ("m" "media")
          ("mt" "tv" entry "* 📺 %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("📼 media")))
          ("mb" "book" entry "* 📕 %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("📼 media")))
          ("mm" "music (is so nice)" entry "* 🎵 %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("📼 media")))
          ("mp" "podcast" entry "* 🎙 %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("📼 media")))

          ("i" "innerspace")
          ("ic" "coffee" entry "* ☕ [[id:88321f53-b156-4254-91b7-4af1359853ca][coffee]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("ie" "third eye" entry "* ☯ [[id:b94c6aad-213c-4091-8275-bfa8c0c363e6][prying open my third eye]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("ii" "innerspace" entry "* ☯ %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("if" "fam" entry "* ❤ %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("ih" "home" entry "* 🏡 [[id:ce6bdbed-76a3-42b6-b614-43438ffd742d][home]] %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("iH" "Health" entry "* ⚖ [[id:721ab13d-c8ff-4f55-8c8f-54687d031fab][weight]] %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("iw" "weight" entry "* 🏥 [[id:3e525893-2e96-401b-bb98-a5a47601192d][health]] %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("im" "metta" entry "* ☯ metta\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("ip" "piracy" entry "* ☠ %?\n%U"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("iv" "vipassana" entry "* ☯ [[id:37187551-897a-4f6a-a978-c057644f34af][vipassana]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("is" "sleep / dreams" entry "* 🌙 sleep / dreams\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))
          ("it" "trees" entry "* 🌳 trees / [[id:cadf3871-658d-4db2-b16c-36aa03dc71dc][brocoli]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("☯ innerspace")))

          ,(my/make-daily-capture "r" "RDV"
                                  "* RDV %? \n<%<%Y-%m-%d>>\n" t)
          ("w" "witness the fitness")
          ("wb" "bouldering" entry "* ⛰ [[id:546e7d60-daa1-413c-96de-a026f4649a17][bouldering]] %? :wtf:\n%U\n** ❤ with :innerspace:\n** 🔥 topped\n** 👷 projects\n** 🏥 [[id:820e7fc2-6cd0-4865-988f-7f526f5545a9][injuries]]\n"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("ws" "sport climbing" entry "* ⛰ [[id:80360dc7-4428-433b-8651-30248b4fe46d][sport climbing]] %? :wtf:\n%U\n** ❤ with :innerspace:\n** 🔥 topped\n** 👷 projects\n** 🏥 [[id:820e7fc2-6cd0-4865-988f-7f526f5545a9][injuries]]\n"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("wf" "fingerboard" entry "* 🤘 [[id:f9bffcdd-13d2-474b-811a-3a8f90458daf][fingerboard]] :wtf:\n%U\n** 🍚 [[id:7acd1856-7e65-4134-b6ec-715976a03e7e][rice bucket]]\n- %?\n** 💪 [[id:0c8446f1-0640-4492-9f2c-5cddfb601a27][pull-ups]]\n** 🐒 [[id:dc388e37-deec-4241-b014-7d99e7f02ad4][campusing]]\n** 🤘 [[id:793f903d-06eb-4368-b057-b30412aff151][deadhangs]]"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("wh" "handstands" entry "** 🤸 [[id:4fd6d453-4088-4182-91b8-ae020d456487][handstands]] :wtf:\n%U *** 🍚 [[id:7acd1856-7e65-4134-b6ec-715976a03e7e][rice bucket]]\n- %?x *** 💪 [[id:5636c886-4684-4343-8797-b46ae4c08301][crow to push butt up high straight back]]\n*** 💪 [[id:ee90c7f5-5ee6-4db4-b613-fccf792e6c2f][baby HSPUs]]\n*** 🤸 [[id:4fd6d453-4088-4182-91b8-ae020d456487][handstands]]\nsession max hold: \n*** 🍌 [[id:6d270194-77c5-4dba-a682-6de4e28ecd38][btw banana stretch]]"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("wr" "rice bucket" entry "* 🍚 [[id:7acd1856-7e65-4134-b6ec-715976a03e7e][rice bucket]] :wtf:\n%U%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("wl" "leg day" entry "* 💪 leg day :wtf:\n%U\n** 🍚 [[id:7acd1856-7e65-4134-b6ec-715976a03e7e][rice bucket]]\n- %?\n** 💪 [[id:12d61817-b852-4ccb-ade2-40b29415694d][cossak hip rotations]]\n** 💪 [[id:a561809c-02eb-4f18-88dc-67eb6810fe8c][pistol squats]]\n"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("wi" "injuries" entry "* 🏥 [[id:820e7fc2-6cd0-4865-988f-7f526f5545a9][injuries]] :wtf:inj:\n%U\n%?\n"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))
          ("ww" "woody" entry "* 📐 [[id:a679eafc-fcaf-404d-a14e-d730a1a5b58b][woody]] :wtf:\n%U\n** 🍚 [[id:7acd1856-7e65-4134-b6ec-715976a03e7e][rice bucket]]\n- %?\n** 🔥 topped\n** 👷 projects\n** 🏥 [[id:820e7fc2-6cd0-4865-988f-7f526f5545a9][injuries]]"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⛰ witness the fitness")))

          ("t" "tech")
          ("tg" "guix" entry "* 🐧 [[id:844ed739-42ce-4277-b7b5-b8f4a79869dc][guix]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⚛ tech")))
          ("te" "emacs" entry "* 🐧 [[id:ab80bf16-aadd-4fa3-a9d6-4d4f7fd1b2e9][emacs]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⚛ tech")))
          ("tl" "linux" entry "* 🐧 [[id:25647313-7296-4803-b2c4-f57b5b6e2d72][linux]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⚛ tech")))
          ("tp" "physics" entry "* ⚛ [[id:a3e5f916-a260-4925-81ef-5f7c6f5e3157][physics]]\n%U\n%?"
           :jump-to-captured t
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("⚛ tech")))


          ("b" "besport")
          ("bd" "BS ssdd top" entry "* [ ] %?"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS ssdd"))
           :prepend t)
          ("bs" "BS ssdd bottom" entry "* [ ] %?"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS ssdd")))
          ("bb" "boop" entry "* boop %? :bs:boop:\n%U\n"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS"))
           :jump-to-captured t)
          ("bl" "backlog prep" entry "* backlog :bs:bl:\n%U\n%?"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS"))
           :jump-to-captured t)
          ("bn" "note" entry "* %? :bs:\n%U"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS"))
           :jump-to-captured t)
          ("br" "réu" entry "* %? :bs:\n%U"
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS"))
           :jump-to-captured t)
          ("ba" "réu appli" entry
           ,(->> (+pass-get-entry "besport/capture/team")
                 (mapcar #'cdr)
                 (-drop 1)
                 (mapcar (lambda (s) (concat s "\n")))
                 (apply #'concat))
           :if-new (file+head+olp ,my/daily-file ,my/daily-header ("🐫 BS"))
           :jump-to-captured t))))

(after! org-capture
  (fuck-me/init-capture))

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
                                                      '((:name "ssdd: daily edition"
                                                         :tag ("ssdd")
                                                         :order 10)
                                                        (:name "ssdd: yolo"
                                                         :tag ("tt" "lol" "yolo")
                                                         :order 20)
                                                        (:name "BS"
                                                         :tag ("bs" "fm" "fuckme")
                                                         :order 30)
                                                        (:name "fun maximization"
                                                         :tag ("fun")
                                                         :order 40)
                                                        (:name "WWSCD"
                                                         :tag ("wwscd")
                                                         :order 50)
                                                        (:name "wtf: focus"
                                                         :and (:tag "wtf" :tag "focus")
                                                         :order 51)
                                                        (:name "wtf"
                                                         :tag ("wtf")
                                                         :order 52)
                                                        (:name "innerspace"
                                                         :tag ("is" "h" "habit" "focus")
                                                         :order 60)
                                                        (:name "review"
                                                         :tag ("review" "r")
                                                         :order 70)
                                                        (:name "next steps"
                                                         :tag "next"
                                                         :order 80)
                                                        (:name "Projects"
                                                         :todo "PROJ"
                                                         :order 90)
                                                        (:name "don't be a cunt"
                                                         :tag "dbac"
                                                         :order 100)
                                                        ;; (:name "repeat after me"
                                                        ;;  :order 9
                                                        ;;  :habit t
                                                        ;;  )
                                                        (:name ".*"
                                                         :order 999
                                                         :anything t)
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

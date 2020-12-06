;; for reference:
;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; lol:

(setq org-roam-directory "~/conf/private/org/here-be-dragons/")
(use-package! org-protocol)

(setq server-name (getenv "EMACS_SERVER"))
(if (string= "DANCE_COMMANDER" server-name)
    (server-start))

;(require 'ol)

(setq fancy-splash-image "~/docs/wallpapers/misc/spock.jpg")
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)
(add-hook '+doom-dashboard-functions #'chika-widget-banner)
(defun chika-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '("                                      :                                 :       "
            "                                    :                                   :       "
            "                                    :  RRVIttIti+==iiii++iii++=;:,       :      "
            "                                    : IBMMMMWWWWMMMMMBXXVVYYIi=;:,        :     "
            "                                    : tBBMMMWWWMMMMMMBXXXVYIti;;;:,,      :     "
            "                                    t YXIXBMMWMMBMBBRXVIi+==;::;::::       ,    "
            "live long & prosper                ;t IVYt+=+iIIVMBYi=:,,,=i+=;:::::,      ;;   "
            "                                   YX=YVIt+=,,:=VWBt;::::=,,:::;;;:;:     ;;;   "
            "                                   VMiXRttItIVRBBWRi:.tXXVVYItiIi==;:   ;;;;    "
            "                                   =XIBWMMMBBBMRMBXi;,tXXRRXXXVYYt+;;: ;;;;;    "
            "                                    =iBWWMMBBMBBWBY;;;,YXRRRRXXVIi;;;:;,;;;=    "
            "                                     iXMMMMMWWBMWMY+;=+IXRRXXVYIi;:;;:,,;;=     "
            "                                     iBRBBMMMMYYXV+:,:;+XRXXVIt+;;:;++::;;;     "
            "                                     =MRRRBMMBBYtt;::::;+VXVIi=;;;:;=+;;;;=     "
            "                                      XBRBBBBBMMBRRVItttYYYYt=;;;;;;==:;=       "
            "                                       VRRRRRBRRRRXRVYYIttiti=::;:::=;=         "
            "                                        YRRRRXXVIIYIiitt+++ii=:;:::;==          "
            "                                        +XRRXIIIIYVVI;i+=;=tt=;::::;:;          "
            "                                         tRRXXVYti++==;;;=iYt;:::::,;;          "
            "                                          IXRRXVVVVYYItiitIIi=:::;,::;          "
            "                                           tVXRRRBBRXVYYYIti;::::,::::          "
            "                                            YVYVYYYYYItti+=:,,,,,:::::;         "
            "                                            YRVI+==;;;;;:,,,,,,,:::::::         "
            ))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                           ?\n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global config:

(setq x-super-keysym 'meta)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package! evil-escape
  :init
  (setq evil-escape-delay 0.3
        evil-escape-key-sequence "jj"
        ;; evil-cross-lines t
        ))

;; (remove-hook 'flyspell-mode-hook #'+spellcheck|immediately)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org files:

(setq org-directory "~/conf/private/org/")
(setq org-roam-directory "~/conf/private/org/here-be-dragons/")
(setq org-journal-dir "~/conf/private/org/the-road-so-far/")
(setq org-agenda-files '("~/conf/private/org/" "~/conf/private/org/people/"
                         "~/conf/private/org/wip/" "~/conf/private/org/work/"
                         "~/conf/private/org/the-road-so-far/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org packages:


;; (use-package! org-roam
;;   :hook (after-init . org-roam-mode))
(use-package! org-roam
  :after org
  :hook (after-init . org-roam-mode)
  )

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
           "[-](S)"   ; Task is in progress
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
  (setq org-log-into-drawer t
        org-auto-align-tags t
        org-tags-column 72))

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

(after! org-capture ;; ?
  (setq org-capture-projects-file "dev"
        ;; org-roam-capture-templates
        ;; `(
        ;;   ("c" "file" entry (function org-roam--capture-get-point)
        ;;    "* %?"
        ;;    :head "#+title: ${title}\n\n"
        ;;    :file-name "%<%F_%H%M%S>-${slug}"
        ;;    :unnarrowed t
        ;;    )

        ;;   ("Q" "Protocol")
        ;;   ("Qq" "Protocol quote" entry
        ;;    (function org-roam--capture-get-point)
        ;;    "* %?\n[[%:link][%:description]]\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
        ;;    :file-name "bookmark/%<%F_%H%M%S>-${slug}"
        ;;    :head "#+title: ${title}\n"
        ;;    :unnarrowed t)
        ;;   ("Ql" "Protocol Link" entry
        ;;    (file+olp "lol.org" "lol" "inbox")
        ;;    "* %?\n[[%:link][%:description]] \n%U")
        ;;   ;:immediate-finish t
        ;;   )
        ;; live with this for a while and then review

        ;; add RDV, project stuff.
        org-capture-templates
        `(("d" "ssdd" entry (function (lambda ()
                                        (my/log-entry '("ssdd"))))

           "* TODO %?\n%t" :prepend t)
          ("s" "ssdd" entry (function (lambda () ;; I appear to prefer s
                                        (my/log-entry '("ssdd"))))

           "* TODO %?\n%t")

          ("w" "log work quick" entry (function (lambda ()
                                            (my/log-entry '("log" "work"))) )
           "* %?\n"
           :no-save t
           :clock-in t)
          ("W" "log work" entry (function (lambda ()
                                                  (my/log-entry '("log" "work"))) )
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)

          ("r" "RDV" entry
           (file+olp ;; FIXME make it scheduled, ask date then time?
            ,(expand-file-name "future.org" org-journal-dir)
            "future" "inbox")
           "* %?\n%^T\n")

          ("t" "todo to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* TODO %?\n%U\n")
          ("n" "note to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %?\n%U\n")
          ("N" "note to inbox" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %?\n%U\n")

          ("f" "Templates for notes from files")
          ("ft" "todo from file" entry
           (file+olp "lol.org" "lol" "inbox")
           "* TODO %?\n%a")
          ("fn" "note from file" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %U %?\n%a")

          ("j" "journal")
          ("jw" "witness the fitness" entry (function (lambda ()
                                                        (my/log-entry '("witness the fitness"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t)

          ("jy" "ty" entry (function (lambda ()
                                       (my/log-entry '("ty"))))
           "* %?\n"
           :jump-to-captured t
           :no-save t)

          ("ji" "innerspace" entry (function (lambda ()
                                               (my/log-entry '("innerspace"))))
           "* %?\n"
           :jump-to-captured t
           :no-save t)


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
           (file+olp "lol.org" "lol" "inbox")
           "* [[%:link][%:description]]\n%U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
           :immediate-finish t)
          ("Ql" "Protocol Link direct" entry
           (file+olp "lol.org" "lol" "inbox")
           "* [[%:link][%:description]]\n%U"
           :immediate-finish t)
          ("QL" "Protocol Link" entry
           (file+olp "lol.org" "lol" "inbox")
           "* %?\n[[%:link][%:description]]\n%U")

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
                                                         :tag ("tt" "bs" "fm" "fuckme" "lol" "yolo" "ssdd")
                                                         :order 1)
                                                        (:name "fun maximization"
                                                         :tag ("fun")
                                                         :order 2)
                                                        (:name "WWSCD"
                                                         :tag ("wwscd")
                                                         :order 3)
                                                        (:name "innerspace"
                                                         :tag ("is" "h" "habit" "focus")
                                                         :order 5)
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
(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

(setq evil-search-wrap nil)
;;
(setq projectile-project-search-path '("~/conf" "~/conf/private" "~/work/2morrow" "~/work/gentoo/overlays" "~/work/ocaml"))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William"
      user-mail-address "john@doe.com")

(setq my/font-size
      (let ((host (system-name)))
        (cond ((string-equal host "daban-urnud") 20)
              ((string-equal host "yggdrasill")  35)
              (t                                 16))))

(setq doom-font (font-spec :family "Fira Mono for Powerline" :size my/font-size))
(setq doom-theme 'doom-solarized-dark)
                                        ; j(setq doom-font (font . "Fira Mono for Powerline-14"))

;; nil numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; (setq scroll-margin 0)

(setq evil-snipe-scope 'whole-visible)

(setq avy-keys '(?u ?h ?e ?t ?. ?c ?i ?d ?k ?m ?j ?w ?o ?n ?p ?g))
(setq avy-all-windows t)

;; parens & clojure:

(use-package! eval-sexp-fu
  ;:defer t
  ;:hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode))
  )
(use-package! cider-eval-sexp-fu)

;; elisp mode wants most of this too:
(after! clojure-mode
  (add-hook 'clojure-mode-hook #'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode) ;; difficult to use with trace-form cljsrn fn tracing
  ;(add-hook 'clojure-mode-hook #'electric-indent-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

;; (setq evil-cleverparens-use-additional-movement-keys nil)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
                                        ;(add-hook 'emacs-lisp-mode-hook #'aggressive-indent)
                                        ;(evil-cleverparens-mode)
;; (after! elisp-mode)
;;
;; TODO: test this: cleverparens
;; (defun my-after-evil ()
;;   (global-evil-surround-mode)
;;   (eyebrowse-mode)
;;   (eyebrowse-setup-opinionated-keys)
;;   (require 'evil-cleverparens-text-objects))
;; (add-hook 'evil-mode-hook 'my-after-evil)

(defun my/reset-paragraph-variables ()
  (kill-local-variable 'paragraph-start)
  (kill-local-variable 'paragraph-separate))
(add-hook 'org-mode-hook 'my/reset-paragraph-variables)


(map! :map evil-cleverparens-mode-map
      :nvm "{" #'evil-backward-paragraph
      :nvm "}" #'evil-forward-paragraph
      :nvm ")" #'evil-cp-next-closing
      :nvm "(" #'sp-backward-up-sexp
      :nvm "é" #'evil-cp-previous-opening ; FIXME put this in global map?
      :nvm "&" #'evil-cp-next-opening
      :nvm "M-r" #'paredit-raise-sexp
      :nvm "M-t"  #'sp-transpose-sexp
      :nvm "M-T"  (lambda() (interactive) (sp-transpose-sexp -1))
      :nvm "M-g p" #'evil-cp-wrap-next-round
      :nvm "M-g P" #'evil-cp-wrap-previous-round
      :nvm "M-g c" #'evil-cp-wrap-next-curly
      :nvm "M-g C" #'evil-cp-wrap-previous-curly
      :nvm "M-g s" #'evil-cp-wrap-next-square
      :nvm "M-g S" #'evil-cp-wrap-previous-square
      ;:nvm "s"  #'evil-aavy-goto-char-2 use: gss
      )

(map! :map evil-cleverparens-mode-map
      :localleader
      :nvm "r"   #'paredit-raise-sexp
      :nvm "R"   #'evil-cp-raise-form
      :nvm "t"   #'sp-transpose-sexp
      :nvm "T"   (lambda() (interactive) (sp-transpose-sexp -1))
      :nvm "M-T" (lambda() (interactive) (sp-transpose-sexp -1))
      :nvm "g p" #'evil-cp-wrap-next-round
      :nvm "g P" #'evil-cp-wrap-previous-round
      :nvm "g c" #'evil-cp-wrap-next-curly
      :nvm "g C" #'evil-cp-wrap-previous-curly
      :nvm "g s" #'evil-cp-wrap-next-square
      :nvm "g S" #'evil-cp-wrap-previous-square
      )

(map! :map clojure-mode-map
      :localleader
      ;:nvm "RET" #'cider-eval-last-sexp
      :nvm "RET" #'cider-eval-list-at-point
      )

;; (map! :map clojure-mode-map
;;       :nvm "s"  #'evil-avy-goto-char-2
;;       )

(map! :map emacs-lisp-mode-map
      :localleader
      :nvm "RET" #'eval-defun)

;; keyboard macros, for reference: https://www.emacswiki.org/emacs/KeyboardMacrosTricks
;; kmacro-name-last-macro
;; insert-kbd-macro
(map! :localleader
      :map org-mode-map
      ;; send title (current line)
      :nvm "D"    #'org-decrypt-entries
      :nvm "ga"   (fset 'archive-send-jt
                        (kmacro-lambda-form [return return ?y ?y ?  ?j ?t ?G ?p ?c ?e ?* ?* ?* ?j ?j return ?t ?d ?o ?\C-c ?. return ?j ?j ?\C-w ?\C-w] 0 "%d"))
      ;; send tree (this sometimes does BS on small lists)
      :nvm "gAr"  (fset 'r-archive-send-jt
                        (kmacro-lambda-form [return return ?y ?a ?r ?  ?j ?t ?G ?p ?c ?e ?* ?* ?* ?j ?j return ?t ?d ?o ?\C-c ?. return ?j ?j ?\C-w ?\C-w] 0 "%d"))
      ;; send tree (this sometimes does BS on small lists) without marking current as done
      :nvm "gAs"  (fset 'r-send-jt
                        (kmacro-lambda-form [?y ?a ?r ?  ?j ?t ?G ?p ?c ?e ?* ?* ?* ?j ?j return ?t ?d ?o ?\C-c ?. return ?j ?j ?\C-w ?\C-w] 0 "%d"))
      ;; make checkbox of this
      :nvm "gt"   (fset 'mk-todo
                        (kmacro-lambda-form [return ?* return ?t ?t ?< ?< ?$] 0 "%d"))
      ;; make todo of this
      :nvm "gT"   (fset 'mk-todo
                        (kmacro-lambda-form [return ?* return ?t ?T ?< ?< ?$] 0 "%d"))
      ;; clock resolve keep
      :nm  "ck"       (fset 'clock-resolve
                            (kmacro-lambda-form [return ?c ?r ?K] 0 "%d"))
      :nvm "RET" #'+org/dwim-at-point
      ;; tables
      :nvm "b>" #'org-table-move-column-right
      :nvm "b<" #'org-table-move-column-left
      :nvm "h" (lambda()
                 (interactive)
                 (org-toggle-heading)
                 (outline-promote))
      ;; dates:
      :desc "inactive timestamp"            :nvm "dn" #'my/insert-inactive-timestamp
      ;; subtree
      :desc "refile copy"                   :nvm "sc" #'org-refile-copy
      )

(map! :map org-mode-map
      ;;:i "RET" #'org-return-and-maybe-indent
      :v   "O"        #'evil-org-open-links
      :nvm "zD"       #'org-decrypt-entries
      :nvm "zq"       #'(lambda() (interactive) (org-show-branches-buffer))
      :nvm "("        #'org-previous-visible-heading
      :nvm ")"        #'org-next-visible-heading
      :nvm "{"        #'evil-backward-paragraph
      :nvm "}"        #'evil-forward-paragraph
      :nvm "$"        #'evil-end-of-line
      :nv   "<left>"  #'org-promote-subtree
      :nv   "<down>"  #'org-move-subtree-down
      :nv   "<up>"    #'org-move-subtree-up
      :nv   "<right>" #'org-demote-subtree)

(defun my/journal-new-todo ()
  (interactive)
  (org-journal-new-scheduled-entry nil (format-time-string "%Y-%m-%d %a" (current-time)))
  (evil-append 0))

(map! :map org-journal-mode-map
      :localleader
      :nvm "n" #'my/journal-new-todo ;; FIXME trigger capture
      :nvm "N" #'org-journal-new-date-entry
      :nvm "r" #'org-journal-new-scheduled-entry
      :nvm "j" #'org-journal-next-entry
      :nvm "k" #'org-journal-previous-entry
      )

;; ocaml
(map! :localleader
      :map tuareg-mode-map
      "RET" #'tuareg-eval-phrase
      "b"   #'tuareg-eval-buffer
      "TAB" #'tuareg-complete
      "K"   #'tuareg-kill-ocaml)

(add-hook 'tuareg-mode-hook #'(lambda() (setq mode-name "🐫")))

;; global:
(map! ;; :nv "s"  #'evil-avy-goto-char-2
      ;; :nv "C->" #'transpose-sexps
 ;; :nv "C-<" #'(lambda() (interactive) (transpose-sexps -1))
 :n   "-d"          #'delete-trailing-whitespace
 :n   "C-S-<left>"  #'winner-undo
 :n   "C-S-<right>" #'winner-redo
 :nv  "C-t"         #'transpose-words
 :nv  "g>"          #'transpose-words
 :nv  "g<"          #'(lambda() (interactive) (transpose-words -1))
 :nv  "C-*"         #'evil-multiedit-match-symbol-and-prev
 :nv  "C-8"         #'evil-multiedit-match-symbol-and-next
 :nvm "é"           #'evil-cp-previous-opening ; FIXME put this in global map?
 :nvm "&"           #'evil-cp-next-opening
 :i   "C-v"         #'evil-paste-after
 :i   "C-V"         #'evil-paste-after)

;; global
(map! :leader
      :nvm "SPC"  #'ivy-switch-buffer
      :nvm "<"    #'+ivy/projectile-find-file
      :nvm "ng"   #'counsel-org-goto-all ;; nG in split buffer?

      :desc "today"              :nvm "jt" #'(lambda() (interactive) (my/journal-open-today))
      :desc "today other window" :nvm "jT" #'(lambda() (interactive) (my/journal-open-today t))

      :nvm "jn" #'my/journal-new-todo ;; FIXME remove/or call capture instead?
      :nvm "jN" #'org-journal-new-entry
      :nvm "jr" #'org-journal-new-scheduled-entry

      :desc "follow" :nvm "taf" #'org-agenda-follow-mode

      ;; maybe not global?
      :desc "inactive timestamp"            :nvm "dn" #'my/insert-inactive-timestamp
      :desc "password-store"                :nvm "P"  #'ivy-pass
      :desc "Switch to buffer"              :nvm "rb" #'org-roam-switch-to-buffer
      :desc "Org Roam Capture"              :nvm "rc" #'org-roam-capture
      :desc "Find file"                     :nvm "rf" #'org-roam-find-file
      :desc "Show graph"                    :nvm "rg" #'org-roam-graph
      :desc "Insert"                        :nvm "ri" #'org-roam-insert
      :desc "Insert (skipping org-capture)" :nvm "rI" #'org-roam-insert-immediate
      :desc "Org Roam"                      :nvm "rr" #'org-roam
      :desc "Roam tag"                      :nvm "rq" #'org-roam-tag-add
      :desc "Arbitrary date"                :nvm "rdd" #'org-roam-dailies-date
      :desc "Today"                         :nvm "rdt" #'org-roam-dailies-today
      :desc "Tomorrow"                      :nvm "rdm" #'org-roam-dailies-tomorrow
      :desc "Yesterday"                     :nvm "rdy" #'org-roam-dailies-yesterday
      )

(map! :map org-agenda-mode-map
      :nvm "j" #'org-agenda-next-line
      :nvm "k" #'org-agenda-previous-line)

;; dired
(map! :localleader
      :map dired-mode-map
      :nvm "RET" #'dired-find-file)


;; helper:
(defun my/keymap-symbol ()
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (interactive)
  (let ((keymap (current-local-map)))
    (print (catch 'gotit
             (mapatoms (lambda (sym)
                   (and (boundp sym)
                        (eq (symbol-value sym) keymap)
                        (not (eq sym 'keymap))
                        (throw 'gotit sym))))))))

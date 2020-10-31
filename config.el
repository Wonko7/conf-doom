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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global config:

(setq x-super-keysym 'meta)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package! evil-escape
  :init
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-key-sequence "jj"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org files:

(setq org-directory "~/conf/private/org/")
(setq org-journal-dir "~/conf/private/org/the-road-so-far/")
(setq org-agenda-files '("~/conf/private/org/" "~/conf/private/org/wip/" "~/conf/private/org/work/" "~/conf/private/org/the-road-so-far/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org packages:

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-compact-blocks t
        org-agenda-start-with-follow-mode t
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
  ;:trigger "__"
  :mode 'org-journal-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org conf:

(after! org
  (setq org-todo-keywords
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

(defun my/journal-open-today ()
  (let ((fpath (expand-file-name (format-time-string "%F_%A.org") org-journal-dir)))
    (find-file fpath)
    (org-decrypt-entries) ;; decrypt org entries before trying to add stuff in them, olp can't work on opaque gpg.
    (when (<= (point-max) 300) ;; FIXME yeahhhhhhh there's probably a better test.
      (message "need init!")
      (print (point-max))
      (org-journal--carryover))
    fpath))

(defun my/log-entry (olp-path)
  (let ((m (org-find-olp (cons (org-capture-expand-file (my/journal-open-today))
                               (cons (format-time-string "%F %A")
                                     olp-path)))))
    (goto-char m)))

(after! org-capture ;; ?
  (setq org-capture-projects-file "dev"
        ;; live with this for a while and then review

        ;; add RDV, project stuff.
        org-capture-templates
        `(("d" "ssdd" entry (function (lambda ()
                                        (my/log-entry '("ssdd"))))

           "* TODO %?\n%t" :prepend t)
          ("s" "ssdd" entry (function (lambda () ;; I appear to prefer s
                                        (my/log-entry '("ssdd"))))

           "* TODO %?\n%t" :prepend t)

          ("w" "log work" entry (function (lambda ()
                                         (my/log-entry '("log" "work"))) )
           "* %?\n"
           :no-save t
           :jump-to-captured t
           :clock-in t)
          ("W" "log work quick" entry (function (lambda ()
                                         (my/log-entry '("log" "work"))) )
           "* %?\n"
           :clock-in t)

          ("r" "RDV" entry
           (file+headline ;; FIXME make it scheduled, ask date then time?
            ,(expand-file-name "future.org" org-journal-dir)
            "inbox")
           "* %?\n%^T\n")

          ("t" "todo to inbox" entry
           (file+headline "lol.org" "inbox")
           "* TODO %?\n%U\n")
          ("n" "note to inbox" entry
           (file+headline "lol.org" "inbox")
           "* %?\n%U\n")
          ("N" "note to inbox" entry
           (file+headline "lol.org" "inbox")
           "* %?\n%U\n")

          ("f" "Templates for notes from files")
          ("ft" "todo from file" entry
           (file+headline "lol.org" "inbox")
           "* TODO %?\n%a")
          ("fn" "note from file" entry
           (file+headline "lol.org" "inbox")
           "* %U %?\n%a")

          ("j" "journal")
          ("jw" "witness the fitness" entry (function (lambda ()
                                                        (my/log-entry '("witness the fitness"))))
           "* %?\n"
           :no-save t
           :jump-to-captured t)

          ("jy" "ty" entry (function (lambda ()
                                       (my/log-entry '("ty"))))
           "* %?\n")

          ("ji" "innerspace" entry (function (lambda ()
                                               (my/log-entry '("innerspace"))))
           "* %?\n")


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
        org-journal-carryover-items "next|TODO=\"PROJ\"|TODO=\"TODO\"|TODO=\"[ ]\"|TODO=\"[ ]\"|TODO=\"\\[ \\]\"|TODO=\"\\[\\\\]\"" ;; checkboxes do not work FIXME
        org-journal-file-format "%F_%A.org"
        org-journal-date-format "%F %A"))

(after! org-agenda
  (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'"
        org-agenda-prefix-format (quote
                                  ((agenda . "%-21c%?-12t% s")
                                   (timeline . "% s")
                                   (todo . "%-20c")
                                   (tags . "%-12c")
                                   (search . "%-12c")))
        org-agenda-deadline-leaders (quote ("!D!: " "D%2d: " ""))
        org-agenda-scheduled-leaders (quote ("" "S%3d: "))
        ;; fixes fucky binding on jk on an agenda header:
        org-super-agenda-header-map (make-sparse-keymap)

        ;; (setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
        ;;       org-agenda-skip-scheduled-if-done t
        ;;       org-agenda-skip-deadline-if-done t
        ;;       org-agenda-include-deadlines t
        ;;       org-agenda-block-separator nil
        ;;       org-agenda-compact-blocks t
        ;;       org-agenda-start-with-log-mode t)
        ;;


        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
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
(setq evil-search-wrap nil)
;;
(setq avy-all-windows t)
(setq projectile-project-search-path '("~/conf" "~/conf/private" "~/work/2morrow" "~/work/gentoo/overlays" "~/work/ocaml"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William"
      user-mail-address "john@doe.com")

(setq doom-theme 'doom-solarized-dark)
(setq doom-font (font-spec :family "Fira Mono for Powerline" :size 16))
; j(setq doom-font (font . "Fira Mono for Powerline-14"))

;; nil numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq scroll-margin 8)

(after! evil-snipe
  (evil-snipe-mode -1))

(setq avy-keys '(?u ?h ?e ?t ?. ?c ?i ?d ?k ?m ?j ?w ?o ?n ?p ?g))

;; parens & clojure:

;; elisp mode wants most of this too:
(after! clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode) ;; difficult to use with trace-form cljsrn fn tracing
  (add-hook 'clojure-mode-hook #'electric-indent-mode)
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
      :nvm ")" #'evil-cp-previous-closing
      :nvm "(" #'evil-cp-next-opening
      :nvm "é" #'evil-cp-previous-opening ; FIXME put this in global map?
      :nvm "&" #'evil-cp-next-opening
      :nvm "M-t"  #'sp-transpose-sexp
      :nvm "M-T"  (lambda() (interactive) (sp-transpose-sexp -1))
      :nvm "M-g p" #'evil-cp-wrap-next-round
      :nvm "M-g P" #'evil-cp-wrap-previous-round
      :nvm "M-g c" #'evil-cp-wrap-next-curly
      :nvm "M-g C" #'evil-cp-wrap-previous-curly
      :nvm "M-g s" #'evil-cp-wrap-next-square
      :nvm "M-g S" #'evil-cp-wrap-previous-square
      :nvm "s"  #'evil-aavy-goto-char-2
      )

(map! :map evil-cleverparens-mode-map
      :localleader
      :nvm "t"  #'sp-transpose-sexp
      :nvm "--T"  (lambda() (interactive) (sp-transpose-sexp -1))
      :nvm "g p" #'evil-cp-wrap-next-round
      :nvm "g P" #'evil-cp-wrap-previous-round
      :nvm "g c" #'evil-cp-wrap-next-curly
      :nvm "g C" #'evil-cp-wrap-previous-curly
      :nvm "g s" #'evil-cp-wrap-next-square
      :nvm "g S" #'evil-cp-wrap-previous-square
      )

(map! :map clojure-mode-map
      :localleader
      :nvm "RET" #'cider-eval-defun-at-point)

(map! :map clojure-mode-map
      :nvm "s"  #'evil-avy-goto-char-2
      )

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
      :nvm "RET" #'+org/dwim-at-point
      ;; tables
      :nvm "b>" #'org-table-move-column-right
      :nvm "b<" #'org-table-move-column-left
      :nvm "h" (lambda()
                 (interactive)
                 (org-toggle-heading)
                 (outline-promote)))

(map! :map org-mode-map
      :nvm "zD"    #'org-decrypt-entries
      :nvm "zq"    #'(lambda() (interactive) (org-show-branches-buffer))
      :nvm "{" #'evil-backward-paragraph
      :nvm "}" #'evil-forward-paragraph
      :nv   "<left>" #'org-promote-subtree
      :nv   "<down>" #'org-move-subtree-down
      :nv   "<up>" #'org-move-subtree-up
      :nv   "<right>" #'org-demote-subtree
      )

(defun my/journal-new-todo ()
  (interactive)
  (org-journal-new-scheduled-entry nil (format-time-string "%Y-%m-%d %a" (current-time)))
  (evil-append 0))

(map! :map org-journal-mode-map
      :localleader
      :nvm "n" #'my/journal-new-todo
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

(add-hook 'tuareg-mode-hook #'rainbow-delimiters-mode)
(add-hook 'tuareg-mode-hook #'(lambda() (setq mode-name "🐫")))

;; global:
(map! :nv "s"  #'evil-avy-goto-char-2
      ;; :nv "C->" #'transpose-sexps
      ;; :nv "C-<" #'(lambda() (interactive) (transpose-sexps -1))
      :nv "g>" #'transpose-words
      :nv "g<" #'(lambda() (interactive) (transpose-words -1))
      :nv "C-*" #'evil-multiedit-match-symbol-and-prev
      :nv "C-8" #'evil-multiedit-match-symbol-and-next
      :i  "C-v" #'evil-paste-after
      :i  "C-V" #'evil-paste-after
      )

;; org
(map! :leader
      :nvm "SPC"  #'ivy-switch-buffer
      :nvm "<"    #'+ivy/projectile-find-file
      :nvm "ng" #'counsel-org-goto-all ;; nG in split buffer?
      :nvm "jt" #'(lambda() (interactive) (my/journal-open-today))
      :nvm "jn" #'my/journal-new-todo ;; FIXME remove/or call capture instead?
      :nvm "jN" #'org-journal-new-entry
      ;; fixme what if day does not exist yet?
      ;; should this be jN new?
      :nvm "jC" (fset 'carry
                      (kmacro-lambda-form [?  ?j ?t ?G ?c ?c ?* ?* ?  ?s ?s ?d ?d ?j ?j ?: ?o ?r ?g ?- ?j ?o ?u ?r ?n ?a ?l ?- ?- ?c ?a ?r ?r ?y ?o ?v ?e ?r return] 0 "%d"))
      :nvm "jr" #'org-journal-new-scheduled-entry
      )

;; dired
(map! :localleader
      :map dired-mode-map
      :nvm "RET" #'dired-find-file)

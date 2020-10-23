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

;; erf, dupe code testing order:

(use-package! org-journal
  :init
  (setq org-journal-dir "~/conf/private/org/the-road-so-far/"
        org-journal-enable-agenda-integration t
        org-journal-enable-cache t
        org-journal-time-format ""
        org-journal-time-prefix "*** "
        org-journal-hide-entries-p nil
        org-journal-carryover-items "next|TODO=\"PROJ\"|TODO=\"TODO\"|TODO=\"[ ]\"|TODO=\"[ ]\"|TODO=\"\\[ \\]\"|TODO=\"\\[\\\\]\"" ;; checkboxes do not work FIXME
        org-journal-file-format "%F_%A.org"
        org-journal-date-format "%F %A"))

(use-package! org-super-agenda
  :after org-agenda
  ;;      ;; org-agenda-skip-scheduled-if-done t
  ;;      ;; org-agenda-skip-deadline-if-done t
  ;;       ;; org-agenda-start-with-log-mode t
  ;;      org-agenda-compact-blocks nil
  ;;      org-agenda-block-separator nil
  ;;       )
  :init
  (setq org-agenda-compact-blocks t
        org-agenda-start-with-follow-mode t
        org-super-agenda-header-separator "\n"
        ;org-super-agenda-header-separator nil
        ;org-agenda-block-separator nil
        )
  :config
  (org-super-agenda-mode))

(setq org-tags-column 72)
(setq org-directory "~/conf/private/org/")
(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(setq org-agenda-files '("~/conf/private/org/" "~/conf/private/org/wip/" "~/conf/private/org/work/" "~/conf/private/org/the-road-so-far/"))

;; org advice newline bug:
;; https://github.com/hlissner/doom-emacs/issues/3172
(setq avy-all-windows t)
(setq projectile-project-search-path '("~/conf" "~/conf/private" "~/work/2morrow" "~/work/gentoo/overlays" "~/work/ocaml"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Fira Mono" :size 14)
;;      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)
(setq doom-font (font-spec :family "Fira Mono for Powerline" :size 16))
; j(setq doom-font (font . "Fira Mono for Powerline-14"))

;; nil numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq scroll-margin 8)

(after! evil-snipe
  (evil-snipe-mode -1))

(setq avy-keys '(?o ?e ?u ?i ?d ?h ?t ?n ?p ?g ?c ?. ?w ?j ?m ?k))

;; parens & clojure:

;; (after! smartparens
;;         ;; (add-hook! clojure-mode #'smartparens-strict-mode)
;;
;;         ;; (setq evil-cleverparens-use-s-and-S nil)
;;
;;         (use-package! evil-cleverparens
;;                       :init
;;                       (setq evil-move-beyond-eol t
;;                             evil-cleverparens-use-additional-bindings nil
;;                             ;; evil-cleverparens-swap-move-by-word-and-symbol t
;;                             ;; evil-cleverparens-use-regular-insert t
;;                             )
;;
;;                       ;; (add-hook! clojure-mode #'evil-cleverparens-mode)
;;                       ;; (add-hook! 'clojure-mode #'rainbow-delimiters-mode
;;                       ;; (add-hook! 'smartparens-enabled-hook #'evil-smartparens-mode)
;;                       ))

;;  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;;  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
;;  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(after! clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode) ;; difficult to use with trace-form cljsrn fn tracing
  (add-hook 'clojure-mode-hook #'electric-indent-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

;; (setq evil-cleverparens-use-additional-movement-keys nil)



(setq org-auto-align-tags t)
(setq org-agenda-prefix-format
      (quote
       ((agenda . "%-20c%?-12t% s")
        (timeline . "% s")
        (todo . "%-20c")
        (tags . "%-12c")
        (search . "%-12c"))))
(setq org-agenda-deadline-leaders (quote ("!D!: " "D%2d: " "")))
(setq org-agenda-scheduled-leaders (quote ("" "S%3d: ")))


(map! :map evil-cleverparens-mode-map
      :nvm "}" #'evil-cp-up-sexp
      :nvm "{" #'evil-cp-backward-up-sexp
      :nvm ")" #'evil-cp-previous-closing
      :nvm "(" #'evil-cp-next-opening
      :nvm "é" #'evil-cp-previous-opening
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

(map! :map clojure-mode-map
      :localleader
      :nvm "RET" #'cider-eval-defun-at-point)

(map! :map clojure-mode-map
      :nvm "s"  #'evil-avy-goto-char-2
      )

;; org
(setq org-use-property-inheritance t) ;; FIXME test this


(map! :localleader
      :map org-mode-map
      :nv "RET" #'+org/dwim-at-point)

(map! :map org-mode-map
      :nv   "<left>" #'org-promote-subtree
      :nv   "<down>" #'org-move-subtree-down
      :nv   "<up>" #'org-move-subtree-up
      :nv   "<right>" #'org-demote-subtree)

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
      :nvm "jt" #'org-journal-open-current-journal-file
      :nvm "jn" #'org-journal-new-entry
      :nvm "jn" #'my/journal-new-todo
      :nvm "jN" #'org-journal-new-date-entry
      :nvm "jr" #'org-journal-new-scheduled-entry
      :nvm "jj" #'org-journal-next-entry
      :nvm "jk" #'org-journal-previous-entry)

;; fixes fucky binding on jk on an agenda header:
(setq org-super-agenda-header-map (make-sparse-keymap))

(let ((org-super-agenda-groups
       '((:name "Projects"
                :children t)
         (:discard (:anything t)))))
  (org-todo-list))

;; wip
;; (defun my/org-journal-on-state-done-return-agenda ()
;;   (save-excursion
;;     (when (or (string= org-state "DONE") (string= org-state "[X]"))
;;       (print (org-heading-components))
;;       (setq init-buffer (current-buffer)) ;; initial buffer
;;       (setq task-title (nth 4 (org-heading-components)))
;;       (print (buffer-file-name))
;;       (print init-buffer)
;;       ;(find-file "~/conf/private/org/the-road-so-far/2020-10-23_Friday.org")
;;       (print task-title )
;;       (org-journal-new-scheduled-entry nil (format-time-string "%Y-%m-%d %a" (current-time)))
;;       (insert task-title)
;;       ;(print task-title)
;;       ;(message task-title)
;;       ;(org-capture nil "j")
;;       ;(insert task-title)
;;       ;(org-capture-finalize)
;;       (switch-to-buffer init-buffer) ;; somehow switch to buffer after C-c C-c is called for the org-capture
;;      )))
;;
;; (add-hook!
;;  'org-after-todo-state-change-hook
;;  #'my/org-journal-on-state-done-return-agenda
;;  )
;; (setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
;;       org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-include-deadlines t
;;       org-agenda-block-separator nil
;;       org-agenda-compact-blocks t
;;       org-agenda-start-with-log-mode t)
;;


(setq org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-include-deadlines t)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
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
                           (:name "Projects"
                            :todo "PROJ"
                            :order 4)
                           (:name "next steps"
                            :tag "next"
                            :order 5)
                           (:name "don't be a cunt"
                            :tag "dbac"
                            :order 6)
                           (:name ".*"
                            :order 7
                            :anything t
                            )
                           ))))))
        ))

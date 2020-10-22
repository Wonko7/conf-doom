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
        org-journal-carryover-items "next|lol|yolo|kkt|work|bs|TODO=\"PROJ\"|TODO=\"TODO\"|TODO=\"[ ]\"|TODO=\"[ ]\"|TODO=\"\\[ \\]\"|TODO=\"\\[\\\\]\"" ;; checkboxes do not work FIXME
        org-journal-file-format "%F_%A.org"
        org-journal-date-format "%F %A"))

;; (setq org-tags-column 72)
;; (after! org
;;   (setq org-tags-column 72))
;;

(use-package! org-super-agenda
  :after org-agenda
  ;;      ;; org-agenda-skip-scheduled-if-done t
  ;;      ;; org-agenda-skip-deadline-if-done t
  ;;       ;; org-agenda-start-with-log-mode t
  ;;       ;;org-agenda-start-with-follow-mode t
  ;;      org-agenda-compact-blocks nil
  ;;      org-agenda-block-separator nil
  ;;       )
  :init
  (setq org-agenda-compact-blocks t
        org-super-agenda-header-separator "\n"
        ;org-super-agenda-header-separator nil
        ;org-agenda-block-separator nil
        )
  :config
  (org-super-agenda-mode))


(setq org-directory "~/conf/private/org/")
(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(setq org-agenda-files '("~/conf/private/org/" "~/conf/private/org/the-road-so-far/"))

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

;; nothing works:
;; (after! org
;;   (setq doom-localleader-key "SPC m")
;;   (evil-set-leader 'normal (kbd "SPC m") t)
;;   )

;; (map! :leader
;;       :map org-mode-map
;;       :nv "m" #'evil-send-localleader)

;; (map! :map general-override-mode-map
;;       :nvm "SPC m" #'doom/localleader)

;; (setq evil-cleverparens-use-additional-movement-keys nil)



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

(map! :map org-journal-mode-map
      :localleader
      :nvm "n" #'org-journal-new-entry
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
      :nvm "jN" #'org-journal-new-date-entry
      :nvm "jr" #'org-journal-new-scheduled-entry
      :nvm "jj" #'org-journal-next-entry
      :nvm "jk" #'org-journal-previous-entry)

(map! :map org-super-agenda-header-map;org-agenda-mode-map ;org-agenda-keymap;evil-org-agenda-mode-map
      ;:nvm "j" #'org-agenda-next-line
      :nvm "j" #'evil-next-line
      :nvm "k" #'evil-previous-line
      )
(map! :map org-agenda-mode-map ;org-agenda-keymap;evil-org-agenda-mode-map
      ;:nvm "j" #'org-agenda-next-line
      :nvm "j" #'evil-next-line
      :nvm "k" #'evil-previous-line
      )
(map! :map org-agenda-keymap;evil-org-agenda-mode-map
      ;:nvm "j" #'org-agenda-next-line
      :nvm "j" #'evil-next-line
      :nvm "k" #'evil-previous-line
      )
(map! :map evil-org-agenda-mode-map
      ;:nvm "j" #'org-agenda-next-line
      :nvm "j" #'evil-next-line
      :nvm "k" #'evil-previous-line
      )

;; evil-org-agenda-mode-map <motion-state> g d
;; org-agenda-keymap j
;; org-agenda-mode-map j
;; org-super-agenda-header-map j
;;
;; wip
;; (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t  ; Items that appear on the time grid
;;                 :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          ;; Set order of multiple groups at once
;;          (:order-multi (2 (:name "Shopping in town"
;;                                  ;; Boolean AND group matches items that match all subgroups
;;                                  :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                                  ;; Multiple args given in list with implicit OR
;;                                  :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                                  :habit t
;;                                  :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                                  ;; Regexps match case-insensitively on the entire entry
;;                                  :and (:regexp ("space" "NASA")
;;                                                ;; Boolean NOT also has implicit OR between selectors
;;                                                :not (:regexp "moon" :tag "planet")))))
;;          ;; Groups supply their own section names when none are given
;;          (:todo "WAITING" :order 8)  ; Set order of this section
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                 ;; Show this group at the end of the agenda (since it has the
;;                 ;; highest number). If you specified this group last, items
;;                 ;; with these todo keywords that e.g. have priority A would be
;;                 ;; displayed in that group instead, because items are grouped
;;                 ;; out in the order the groups are listed.
;;                 :order 9)
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda-list))

(let ((org-super-agenda-groups
       '((:name "Projects"
                :children t)
         (:discard (:anything t)))))
  (org-todo-list))


;; (setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
;;       org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-include-deadlines t
;;       org-agenda-block-separator nil
;;       org-agenda-compact-blocks t
;;       org-agenda-start-with-log-mode t)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "" )))
        ("z" "Super zaen view"
         ((agenda "" )
          (alltodo "=" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "ssdd"
                            :tag ("tt" "fm" "fuckme" "lol" "yolo" "ssdd")
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
                           (:name "don't be a cunt"
                            :tag "dbac"
                            :order 5)
                           (:name ".*"
                            :order 6
                            :anything t
                            )
                           ))))))
        ))

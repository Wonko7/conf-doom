;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
(setq x-super-keysym 'meta)


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package! evil-escape
  :init
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-key-sequence "jj"))

(setq avy-all-windows t)
(setq projectile-project-search-path '("~/conf" "~/conf/private" "~/work"))

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
;; jj(setq doom-font (font-spec :family "Fira Mono" :size 14)
;; jj     )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)
(setq doom-font (font-spec :family "Fira Mono for Powerline" :size 16))
; j(setq doom-font (font . "Fira Mono for Powerline-14"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (map! ;:map general-override-mode-map
      ;:nvm "<newleaderkey"> #'doom/leader
      ;:ei "<alternative-newleaderkey"> #'doom/leader
      ;; "s" 'avy-goto-char-2
      ;; )

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
  ;(add-hook 'clojure-mode-hook #'aggressive-indent-mode) ;; difficult to use with trace-form cljsrn fn tracing
  (add-hook 'clojure-mode-hook #'electric-indent-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

;; nothing works:
;; (after! org
;;   (setq doom-localleader-key "SPC m")
;;   (evil-set-leader 'normal (kbd "SPC m") t))
;; (map! :leader
;;       :map org-mode-map
;;       "m" #'evil-send-localleader)
;; (map! :localleader
;;       :map org-mode-map
;;       "RET" #'org/dwim-at-point)


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
(map! :nv "s"  #'evil-avy-goto-char-timer
      :nv "g>" #'transpose-words
      :nv "g<" #'(lambda() (interactive) (transpose-words -1))
      )

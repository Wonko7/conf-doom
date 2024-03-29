;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; pinned org-roam was too slow on startup
;(package! breadcrumb)
(package! evil-cleverparens)
(package! aggressive-indent)
;(package! auth-source)

(package! org-super-agenda)
;; (package! org-journal)
;; (package! org-roam :pin "f819720c5101")
(package! org-ql)
(package! org-web-tools)

(package! writeroom-mode)

;(package! org-protocol)
;(package! org-plus-contrib)

(package! eval-sexp-fu)
(package! cider-eval-sexp-fu)
(package! rainbow-identifiers)

(package! graphql-mode)
(package! prettier-js)

; nope these should come from opam:
(package! ocamlformat)
;(package! merlin)
;(package! dune)

(package! highlight-indent-guides)
;; (package! company-fuzzy)
;; (package! flx)

(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))
(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))

(package! dogears
  :recipe (:host github :repo "alphapapa/dogears.el"))

(package! magit-annex)

(package! websocket)
;; (package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; (package! org-roam-timestamps :recipe (:host github :repo "ThomasFKJorna/org-roam-timestamps"))
;; ^ was buggy

(package! elpher)
;; (package! gnutls)

;; (package! origami)
;; (package! foldout)

;; (package! debbugs)

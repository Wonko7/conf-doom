;;; ../conf/doom/fancy.el -*- lexical-binding: t; -*-


(setq fancy-splash-image "/data/docs/pics/web-stuff/spock.jpg")
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)
(add-hook '+doom-dashboard-functions #'spock-widget-banner)

(defun spock-widget-banner ()
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

(setq doom-modeline-height 40)

;; fonts & faces

(set-face-attribute 'nobreak-space nil :underline t)
(setq x-super-keysym 'meta)

(defun fuck-me/init-faces ()
  (custom-set-faces!
;;    '(caml-types-expr-face     :background "#072530")
    '(lsp-face-highlight-textual :foreground unspecified :background nil :underline (:style line :color "#d33682")) ;; ping
    ;'(lsp-face-highlight-textual :foreground unspecified :background nil :underline (:style line :color "#d33682")) ;; ping
    ;'(lsp-face-highlight-textual :foreground unspecified :background nil :underline (:style line :color "#268bd2")) ;; blue
    '(caml-types-expr-face :underline (:style line :color "#072530"))
    '(flycheck-error     :underline (:style line :color "#dc322f"))
    '(flycheck-info      :underline (:style line :color "#859900"))
    '(flycheck-warning   :underline (:style line :color "#b58900"))
    '(flyspell-duplicate :underline (:style line :color "#b58900"))
    '(flyspell-incorrect :underline (:style line :color "#dc322f"))))

(fuck-me/init-faces)

(setq my/font-size
      (let ((host (system-name)))
        (cond ((string-equal host "daban-urnud") 20)
              ((string-equal host "yggdrasill")  35)
              ((string-equal host "enterprise")  16)
              (t                                 16))))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size my/font-size))
(setq use-default-font-for-symbols t)
(set-fontset-font t 'symbol "NotoEmoji Nerd Font Mono" nil 'append)
;; (set-fontset-font t 'symbol "NotoEmoji Nerd Font Mono" nil 'append)
;; (set-fontset-font t 'symbol "Twitter Color Emoji" nil 'append)
;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿 🖖

(setq doom-theme 'doom-solarized-dark)

(defun fuck-me/init-font-symbols ()
  (set-fontset-font t 'symbol "NotoEmoji Nerd Font Mono" nil 'append)
  t)

;;; ../conf/doom/communication.el -*- lexical-binding: t; -*-

(after! circe
  (setq irc-debug-log t)

  ;; no DCC in circe so fuck me.
  ;; (set-irc-server! "irc.irchighway.net"
  ;;   `(; :tls t
  ;;     :port 6666
  ;;     :nick ,(+pass-get-user "irc/irchighway.net")
  ;;     :channels ("#ebooks"
  ;;                ;;"##space" "##philosophy"
  ;;                )))

  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick ,(+pass-get-user "irc/libera.chat")
      :sasl-username ,(+pass-get-user "irc/libera.chat")
      :sasl-password (lambda (&rest _) (+pass-get-secret "irc/libera.chat"))
      :channels (:after-auth
                 "#ocaml" "#guile" "#haskell" "#clojure"
                 "#emacs"
                 "#gentoo" "#gentoo-chat" "#guix" "#nonguix"
                 "##space" "##philosophy"
                 ))))

;;; ../conf/doom/yolobolo.el -*- lexical-binding: t; -*-

(require 'org-protocol)
(require 'ol) ;; for org-link-decode

(defun insert-tab-entry (title url indentation-level)
  (let ((stars     (make-string indentation-level ?*))
        (timestamp (format-time-string "[%F %a %H:%M]\n")))
    (insert stars " [[" url "][" title "]]\n"
            timestamp "\n"
            ":PROPERTIES:\n"
            ":DOMAIN:   " (replace-regexp-in-string "^\\(.*://[^/]*\\).*$" "\\1" url) "\n"
            ":URL:      " url "\n"
            ":TITLE:    " title "\n"
            ":DATE:     " timestamp "\n"
            ":END:\n"
            )))

;;;; Functions
(defun yolobolo-save (tab-list)
  (interactive "P")
  (message "hola")
  (save-excursion
    (let ((session (cadr tab-list))
          (file   (concat org-directory  (format-time-string "browsing/%F_%A.org"))))
      ;; goto correct point:
      (with-current-buffer (find-file-noselect file)
        (goto-char (max-char))
        ;; insert session header
        (insert "* " session "   :browsing-session:\n")
        ;; insert each tab
        (cl-loop for (_ title _ url) in (seq-partition (cddr tab-list) 6)
                 do (insert-tab-entry title url 2)))
      (message "captured session %s" session)
      nil)))

;; so the filtering of data should happen in emacs really. ff dumps max data.
;; so. column viewer might be helpful here.
;; can I filter by domain?
;; start thinking about what we can do with org-ql next.
(push '("lol-yolobolo" :protocol "yolobolo" :function yolobolo-save)
      org-protocol-protocol-alist)


;; (setq lol '(:lol "haha" :title "(2) Aaron Smith - Dancin (KRONO Remix) - YouTube" :url "https://www.youtube.com/watch?v=0XFudmaObLI&app=desktop" :title "(1) YouTube" :url "https://www.youtube.com/" :title "(2) George Orwell, Aldous Huxley : \"1984\" ou \"Le meilleur des mondes\" ? | ARTE - YouTube" :url "https://www.youtube.com/watch?v=8lHcq-jaMr0" :title "(2) The Way to Change #1- The Journey - YouTube" :url "https://www.youtube.com/watch?v=fW69HBiu2Ic" :title "(2) Rotpunkt: Bibliographie | Alex Megos climbs his hardest project yet - YouTube" :url "https://www.youtube.com/watch?v=COuxNFuAS1Q" :title "(2) The Hardest Trad Climb In The World? | Climbing Daily Ep.1753 - YouTube" :url "https://www.youtube.com/watch?v=RPbJ-3Y_gaw" :title "Joe Rogan Experience #1540 - Frank von Hippel - YouTube" :url "https://www.youtube.com/watch?v=Kv3dsPkTvhY" :title "Joe Rogan Experience #1551 - Paul Saladino - YouTube" :url "https://www.youtube.com/watch?v=s8tJ-R28HX8" :title "Joe Rogan Experience #1564 - Adam Alter - YouTube" :url "https://www.youtube.com/watch?v=J68gMp9nVE0" :title "Joe Rogan Experience #1560 - Mike Baker - YouTube" :url "https://www.youtube.com/watch?v=jH4dl2coorM" :title "\"Maid\" you look! Oc-Maid by Sweethoneybunnyyy : cosplaybabes" :url "https://www.reddit.com/r/cosplaybabes/comments/jv12cf/maid_you_look_ocmaid_by_sweethoneybunnyyy/" :title "Kid CuDi Up, Up And Away - YouTube" :url "https://www.youtube.com/watch?v=qRPpLGxGNZY" :title "FREESKATE LONDON - Morning ride and visit to the markets (Inline Skating Flow) - YouTube" :url "https://www.youtube.com/watch?v=NFALzzTQAzg" :title "Etienne Klein : « Les ingénieurs franchement on ne les entend pas beaucoup » - YouTube" :url "https://www.youtube.com/watch?v=l_WZDVAX6mQ" :title "Crew-1 Mission - YouTube" :url "https://www.youtube.com/watch?v=bnChQbxLkkI" :title "No Country for Old Men and There Will Be Blood seem connected at the hip to me for some reason. Is this common? Do you make the same connection? : movies" :url "https://www.reddit.com/r/movies/comments/jucqf2/no_country_for_old_men_and_there_will_be_blood/" :title "How Large Can a Bacteria get? Life & Size 3 - YouTube" :url "https://www.youtube.com/watch?v=E1KkQrFEl2I" :title "How do you organize your daily tasks with org-mode? : emacs" :url "https://www.reddit.com/r/emacs/comments/9nbxe8/how_do_you_organize_your_daily_tasks_with_orgmode/" :title "(4) Progressive Psytrance mix November 2019 - YouTube" :url "https://www.youtube.com/watch?v=jJScvRnK2rk" :title "(4) Century Media Records - YouTube" :url "https://www.youtube.com/c/centurymedia/search?query=i%20am%20legion" :title "My hardest climbing session yet! - YouTube" :url "https://www.youtube.com/watch?v=3e_DbnS7yS0" :title "Pierre de Lune (en traversée) (7A+), Le Calvaire, Fontainebleau - YouTube" :url "https://www.youtube.com/watch?v=YtuI6xQl1pk" :title "(4) When you lose Pennsylvania - YouTube" :url "https://www.youtube.com/watch?v=RNc4wooOkJs&feature=youtu.be" :title "(1) The man that sounded the Coronavirus alarm: Joscha Bach @Plinz | The Vance Crowe Podcast - YouTube" :url "https://www.youtube.com/watch?v=yNDj0YN1SH4" :title "(1) Joscha: Computational Meta-Psychology - YouTube" :url "https://www.youtube.com/watch?v=WRdJCFEqFTU" :title "Professeur Raoult (7A), Le Calvaire, Fontainebleau - YouTube" :url "https://www.youtube.com/watch?v=7uAb83xk_8o" :title "(4) Pierre de Lune (assis) (7A), Le Calvaire, Fontainebleau - YouTube" :url "https://www.youtube.com/watch?v=_lrQh31LPA0" :title "Eugenia Kuyda: Friendship with an AI Companion | Lex Fridman Podcast #121 - YouTube" :url "https://www.youtube.com/watch?v=_AGPbvCDBCk" :title "(4) Sara Seager: Search for Planets and Life Outside Our Solar System | Lex Fridman Podcast #116 - YouTube" :url "https://www.youtube.com/watch?v=-jA2ABHBc6Y" :title "(4) Sheldon Solomon: Death and Meaning | Lex Fridman Podcast #117 - YouTube" :url "https://www.youtube.com/watch?v=qfKyNxfyWbo" :title "(4) Highball: Nico Pelorson et Lucien Martinez s'attaquent au terrible Démonia ! | Relais Vertical #120 - YouTube" :url "https://www.youtube.com/watch?v=Ogud6en9jXM" :title "(4) Anton Fomenko - YouTube" :url "https://www.youtube.com/c/AntonFomenko/videos" :title "(4) Towards understanding consciousness in computational systems. Part2. Joscha Bach. - YouTube" :url "https://www.youtube.com/watch?v=rpCoFpWq9gM" :title "Ep. 6 - Awakening from the Meaning Crisis - Aristotle, Kant, and Evolution - YouTube" :url "https://www.youtube.com/watch?v=A_gH5VIZO0Q" :title "(3) CityBlades Tutorial - Intro to Down Hill - YouTube" :url "https://www.youtube.com/watch?v=WYZQ8DNipIk" :title "YouTube" :url "https://www.youtube.com/" :title "Cognitive Science Rescues the Deconstructed Mind | John Vervaeke | TEDxUofT - YouTube" :url "https://www.youtube.com/watch?v=czddkPxz4K4" :title "The Pinebook Pro Is The Affordable Linux Laptop We've Been Waiting For - YouTube" :url "https://www.youtube.com/watch?v=l6dGeRUt4dg" :title "(2) YouTube" :url "https://www.youtube.com/" :title "(2) YouTube" :url "https://www.youtube.com/" :title "Whitney Cummings: Comedy, Robotics, Neurology, and Love | Lex Fridman Podcast #55 - YouTube" :url "https://www.youtube.com/watch?v=0-3kw5BEKB8" :title "How Many Lache Transitions Can We Do? - YouTube" :url "https://www.youtube.com/watch?v=u_P2rGkaw0s" :title "The Power Of Jam Tram - YouTube" :url "https://www.youtube.com/watch?v=tOIr1An3Nvk" :title "Climbing past the NO FALL ZONE! | Day out climbing in Scotland - YouTube" :url "https://www.youtube.com/watch?v=9eL2CoWOdB8" :title "German Bouldering Championship 2020 - Finals - YouTube" :url "https://www.youtube.com/watch?v=RpLEPtrIvkw" :title "Etienne Klein : « Les ingénieurs franchement on ne les entend pas beaucoup » - YouTube" :url "https://www.youtube.com/watch?v=l_WZDVAX6mQ" :title "Ben Goertzel: Artificial General Intelligence | Lex Fridman Podcast #103 - YouTube" :url "https://www.youtube.com/watch?v=OpSmCKe27WE" :title "Your first extension - Mozilla | MDN" :url "https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension" :title "Star Lapse Movement - Insane Handbalancing, Calisthenics and movement - The Raw Movement series - YouTube" :url "https://www.youtube.com/watch?v=en1HFEQmSbY" :title "FREE Calisthenics & Movement Follow-along Class - Best handstand drills & push strength - YouTube" :url "https://www.youtube.com/watch?v=ikvsxl_u6Vw"))
;; (yolobolo-save lol)

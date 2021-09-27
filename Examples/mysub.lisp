(ql:quickload "claraoke-user" :silent t)

(in-package :claraoke-user)

(defparameter *mysub* (subtitle "Eyes on Me" :text nil :fontsize 54 :alignment 5 :italic -1 :fontname "DejaVu Serif"))

(setf (interval *mysub*) 60)
(setf (interval-frequency *mysub*) 8)

;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line01* (dialogue "Whenever sang my songs" :generate-overrides-p t :spell-duration 30))
(defparameter *line02* (dialogue "On the stage, on my own" :generate-overrides-p t :spell-duration 30))
(defparameter *line03* (dialogue "Whenever said my words" :generate-overrides-p t :spell-duration 30))
(defparameter *line04* (dialogue "Wishing they would be heard" :generate-overrides-p t :spell-duration 30))
(defparameter *line05* (dialogue "I saw you smiling at me" :generate-overrides-p t :spell-duration 30))
(defparameter *line06* (dialogue "Was it real or just my fantasy" :generate-overrides-p t :spell-duration 30))
(defparameter *line07* (dialogue "You'd always be there in the corner" :generate-overrides-p t :spell-duration 30))
(defparameter *line08* (dialogue "Of this tiny little bar" :generate-overrides-p t :spell-duration 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line09* (dialogue "My last night here for you" :generate-overrides-p t :spell-duration 30))
(defparameter *line10* (dialogue "Same old songs, just once more" :generate-overrides-p t :spell-duration 30))
(defparameter *line11* (dialogue "My last night here with you?" :generate-overrides-p t :spell-duration 30))
(defparameter *line12* (dialogue "Maybe yes, maybe no" :generate-overrides-p t :spell-duration 30))
(defparameter *line13* (dialogue "I kind of liked it your way" :generate-overrides-p t :spell-duration 30))
(defparameter *line14* (dialogue "How you shyly placed your eyes on me" :generate-overrides-p t :spell-duration 30))
(defparameter *line15* (dialogue "Oh, did you ever know?" :generate-overrides-p t :spell-duration 30))
(defparameter *line16* (dialogue "That I had mine on you" :generate-overrides-p t :spell-duration 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line17* (dialogue "Darling, so there you are" :generate-overrides-p t :spell-duration 30))
(defparameter *line18* (dialogue "With that look on your face" :generate-overrides-p t :spell-duration 30))
(defparameter *line19* (dialogue "As if you're never hurt" :generate-overrides-p t :spell-duration 30))
(defparameter *line20* (dialogue "As if you're never down" :generate-overrides-p t :spell-duration 30))
(defparameter *line21* (dialogue "Shall I be the one for you" :generate-overrides-p t :spell-duration 30))
(defparameter *line22* (dialogue "Who pinches you softly but sure" :generate-overrides-p t :spell-duration 30))
(defparameter *line23* (dialogue "If frown is shown then" :generate-overrides-p t :spell-duration 30))
(defparameter *line24* (dialogue "I will know that you are no dreamer" :generate-overrides-p t :spell-duration 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line25* (dialogue "So let me come to you" :generate-overrides-p t :spell-duration 30))
(defparameter *line26* (dialogue "Close as I wanna be" :generate-overrides-p t :spell-duration 30))
(defparameter *line27* (dialogue "Close enough for me" :generate-overrides-p t :spell-duration 30))
(defparameter *line28* (dialogue "To feel the heart beating fast" :generate-overrides-p t :spell-duration 30))
(defparameter *line29* (dialogue "And stay there as I whisper" :generate-overrides-p t :spell-duration 30))
(defparameter *line30* (dialogue "How I loved your peaceful eyes on me" :generate-overrides-p t :spell-duration 30))
(defparameter *line31* (dialogue "Did you ever know" :generate-overrides-p t :spell-duration 30))
(defparameter *line32* (dialogue "That I had mine on you" :generate-overrides-p t :spell-duration 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line33* (dialogue "Darling, so share with me" :generate-overrides-p t :spell-duration 30))
(defparameter *line34* (dialogue "Your love if you have enough" :generate-overrides-p t :spell-duration 30))
(defparameter *line35* (dialogue "Your tears if you're holding back" :generate-overrides-p t :spell-duration 30))
(defparameter *line36* (dialogue "Or pain if that's what it is" :generate-overrides-p t :spell-duration 30))
(defparameter *line37* (dialogue "How can I let you know" :generate-overrides-p t :spell-duration 30))
(defparameter *line38* (dialogue "I'm more than the dress and the voice" :generate-overrides-p t :spell-duration 30))
(defparameter *line39* (dialogue "Just reach me out then" :generate-overrides-p t :spell-duration 30))
(defparameter *line40* (dialogue "You will know that you're not dreaming" :generate-overrides-p t :spell-duration 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *line41* (dialogue "Darling, so there you are" :generate-overrides-p t :spell-duration 30))
(defparameter *line42* (dialogue "With that look on your face" :generate-overrides-p t :spell-duration 30))
(defparameter *line43* (dialogue "As if you're never hurt" :generate-overrides-p t :spell-duration 30))
(defparameter *line44* (dialogue "As if you're never down" :generate-overrides-p t :spell-duration 30))
(defparameter *line45* (dialogue "Shall I be the one for you" :generate-overrides-p t :spell-duration 30))
(defparameter *line46* (dialogue "Who pinches you softly but sure" :generate-overrides-p t :spell-duration 30))
(defparameter *line47* (dialogue "If frown is shown then" :generate-overrides-p t :spell-duration 30))
(defparameter *line48* (dialogue "I will know that you are no dreamer" :generate-overrides-p t :spell-duration 30))

;; Overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 01. "Whenever sang my songs"
(update-karaoke (find-override *line01* "ne") 90)
(update-karaoke (find-override *line01* "sang") 90)
;; 02. "On the stage, on my own"
(update-karaoke (find-override *line02* "ge,") 150)
(update-karaoke (find-override *line02* "my") 90)
;; 03. "Whenever said my words"
(update-karaoke (find-override *line03* "ver") 90)
(update-karaoke (find-override *line03* "said") 60)
;; 04. "Wishing they would be heard"
(update-karaoke (find-override *line04* "they") 90)
;; 05. "I saw you smiling at me"
(update-karaoke (find-override *line05* "saw") 60)
(update-karaoke (find-override *line05* "ling") 90)
(update-karaoke (find-override *line05* "at") 60)
;; 06. "Was it real or just my fantasy"
(update-karaoke (find-override *line06* "real") 60)
(update-karaoke (find-override *line06* "just") 60)
(update-karaoke (find-override *line06* "my") 90)
;; 07. "You'd always be there in the corner"
(update-karaoke (find-override *line07* "ways") 90)
;; 08. "Of this tiny little bar"
(update-karaoke (find-override *line08* "this") 90)
(update-karaoke (find-override *line08* "ti") 180)
(increase-override (find-override *line08* "ny"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 09. "my last night here for you"
(update-karaoke (find-override *line09* "night") 90)
;; 10. "same old songs,s just once more"
(update-karaoke (find-override *line10* "songs,") 150)
;; 11. "my last night here with you?"
(update-karaoke (find-override *line11* "last") 60)
(update-karaoke (find-override *line11* "night") 90)
(update-karaoke (find-override *line11* "re") 30)
;; 12. "maybe yes, maybe no"
(update-karaoke (find-override *line12* "yes,") 90)
;; 13. "i kind of liked it your way"
(update-karaoke (find-override *line13* "kind") 90)
;; 14. "how you shyly placed your eyes on me"
(delete-override *line14* "ced")
(update-karaoke (find-override *line14* "shy") 120)
(update-karaoke (find-override *line14* "on") 60)
;; 15. "oh, did you ever know?"
(update-karaoke (find-override *line15* "did") 90)
(update-karaoke (find-override *line15* "you") 90)
;; 16. "that i had mine on you"
(update-karaoke (find-override *line16* "ne") 210)
(update-karaoke (find-override *line16* "on") 60)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 17. "Darling, so there you are"
(update-karaoke (find-override *line17* "re") 120)
;; 18. "With that look on your face"
;; 19. "As if you're never hurt"
(update-karaoke (find-override *line19* "ne") 90)
;; 20. "As if you're never down"
(update-karaoke (find-override *line20* "ne") 90)
;; 21. "Shall I be the one for you"
(update-karaoke (find-override *line21* "I") 60)
(update-karaoke (find-override *line21* "one") 120)
;; 22. "Who pinches you softly but sure"
(update-karaoke (find-override *line22* "you") 60)
(update-karaoke (find-override *line22* "ly") 150)
(update-karaoke (find-override *line22* "but") 90)
;; 23. "If frown is shown then"
(update-karaoke (find-override *line23* "shown") 60)
;; 24. "I will know that you are no dreamer"
(delete-override *line24* "will")
(update-karaoke (find-override *line24* "know") 90)
(update-karaoke (find-override *line24* "that") 60)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 25. "So let me come to you"
(delete-override *line25* "me to")
(update-karaoke (find-override *line25* "let") 90)
(update-karaoke (find-override *line25* "me") 90)
;; 26. "Close as I wanna be"
(delete-override *line26* "se")
(update-karaoke (find-override *line26* "I") 150)
;; 27. "Close enough for me"
(delete-override *line27* "se")
(update-karaoke (find-override *line27* "Close") 150)
(update-karaoke (find-override *line27* "enough") 90)
;; 28. "To feel the heart beating fast"
(update-karaoke (find-override *line28* "heart") 120)
(update-karaoke (find-override *line28* "beating") 30)
;; 29. "And stay there as I whisper"
(update-karaoke (find-override *line29* "I") 90)
;; 30. "How I loved your peaceful eyes on me"
(delete-override *line30* "ved")
(delete-override *line30* "ful")
(update-karaoke (find-override *line30* "loved") 90)
(update-karaoke (find-override *line30* "on") 90)
;; 31. "Did you ever know"
(update-karaoke (find-override *line31* "you") 120)
;; 32. "That I had mine on you"
(update-karaoke (find-override *line32* "ne") 210)
(update-karaoke (find-override *line32* "on") 60)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 33. "Darling, so share with me"
(update-karaoke (find-override *line33* "re") 120)
;; 34. "Your love if you have enough"
(delete-override *line34* "ve i")
;; 35. "Your tears if you're holding back"
(update-karaoke (find-override *line35* "re") 90)
;; 36. "Or pain if that's what it is"
;; 37. "How can I let you know"
(update-karaoke (find-override *line37* "let") 90)
;; 38. "I'm more than the dress and the voice"
(update-karaoke (find-override *line38* "dress") 90)
(update-karaoke (find-override *line38* "and") 90)
(update-karaoke (find-override *line38* "the") 90)
;; 39. "Just reach me out then"
;; 40. "You will know that you're not dreaming"
(update-karaoke (find-override *line40* "know") 90)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 41. "Darling, so there you are"
(update-karaoke (find-override *line41* "re") 120)
;; 42. "With that look on your face"
;; 43. "As if you're never hurt"
(update-karaoke (find-override *line43* "ne") 90)
;; 44. "As if you're never down"
(update-karaoke (find-override *line44* "ne") 90)
;; 45. "Shall I be the one for you"
(update-karaoke (find-override *line45* "I") 60)
(update-karaoke (find-override *line45* "one") 120)
;; 46. "Who pinches you softly but sure"
(update-karaoke (find-override *line46* "you") 60)
(update-karaoke (find-override *line46* "ly") 150)
(update-karaoke (find-override *line46* "but") 90)
;; 47. "If frown is shown then"
(update-karaoke (find-override *line47* "shown") 60)
;; 48. "I will know that you are no dreamer"
(delete-override *line48* "will")
(update-karaoke (find-override *line48* "know") 90)
(update-karaoke (find-override *line48* "that") 60)

;; Add lines with counter ( Autoincrement )
(setf (interval-counter *mysub*) (durationinteger "27.60"))
(insert-event *mysub* *line01* :interval-delay 0)
(insert-event *mysub* *line02*)
(insert-event *mysub* *line03*)
(insert-event *mysub* *line04*)
(insert-event *mysub* *line05*)
(insert-event *mysub* *line06* :interval-delay 0)
(insert-event *mysub* *line07*)
(insert-event *mysub* *line08* :interval-delay 0 :interval-event 10)
(setf (interval-counter *mysub*) (durationinteger "1:10.40"))
(insert-event *mysub* *line09* :interval-delay 0)
(insert-event *mysub* *line10*)
(insert-event *mysub* *line11*)
(insert-event *mysub* *line12*)
(insert-event *mysub* *line13*)
(insert-event *mysub* *line14* :interval-delay 0)
(insert-event *mysub* *line15*)
(insert-event *mysub* *line16* :interval-delay 0)
(setf (interval-counter *mysub*) (durationinteger "1:54.60"))
(insert-event *mysub* *line17* :interval-delay 0)
(insert-event *mysub* *line18*)
(insert-event *mysub* *line19*)
(insert-event *mysub* *line20*)
(insert-event *mysub* *line21* :interval-delay 0)
(insert-event *mysub* *line22* :interval-event 11)
(insert-event *mysub* *line23* :interval-delay 0 :interval-event 5)
(insert-event *mysub* *line24* :interval-delay 0 :interval-event 16)
(setf (interval-counter *mysub*) (durationinteger "3:4.80"))
(insert-event *mysub* *line25* :interval-delay 0)
(insert-event *mysub* *line26*)
(insert-event *mysub* *line27*)
(insert-event *mysub* *line28*)
(insert-event *mysub* *line29*)
(insert-event *mysub* *line30*)
(insert-event *mysub* *line31* :interval-event 7)
(insert-event *mysub* *line32* :interval-delay 0)
(setf (interval-counter *mysub*) (durationinteger "3:49.40"))
(insert-event *mysub* *line33* :interval-delay 0)
(insert-event *mysub* *line34*)
(insert-event *mysub* *line35*)
(insert-event *mysub* *line36* :interval-delay 0 :interval-event 9)
(insert-event *mysub* *line37*)
(insert-event *mysub* *line38* :interval-delay 0 :interval-event 10)
(insert-event *mysub* *line39* :interval-event 5)
(insert-event *mysub* *line40* :interval-delay 0 :interval-event 10)
(setf (interval-counter *mysub*) (durationinteger "4:32.40"))
(insert-event *mysub* *line41* :interval-delay 0)
(insert-event *mysub* *line42*)
(insert-event *mysub* *line43*)
(insert-event *mysub* *line44*)
(insert-event *mysub* *line45* :interval-delay 0)
(insert-event *mysub* *line46* :interval-event 11)
(insert-event *mysub* *line47* :interval-delay 0 :interval-event 5)
(insert-event *mysub* *line48* :interval-delay 0 :interval-event 16)

;; Disable counter
(setf (interval-counter *mysub*) nil)

(insert-style *mysub* (style "Title" :alignment 8 :shadow 2 :outline 0 :back-colour "&HA0D4FF7F" :primary-colour "&H30FFFFFF" :fontname "DejaVu San"))
(insert-style *mysub* (style "Notes" :alignment 8 :shadow 2 :outline 0 :back-colour "&HA0D4FF7F" :primary-colour "&H30C36888" :fontname "DejaVu San"))

(insert-event *mysub* (dialogue "Eyes on Me - Faye Wong" :style "Title" :end "00:05:38"))
(insert-event *mysub* (dialogue (claraoke-internal:version) :style "Notes" :end "00:05:38"))

;; Write to file
(claraoke-subtitle-to-file *mysub* #p"/tmp/mysub.ass")


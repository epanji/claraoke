(ql:quickload "claraoke-user" :silent t)

(in-package :claraoke-user)

(defparameter *mysub* (subtitle "Eyes on Me" :text nil :fontsize 54 :alignment 5 :italic -1 :fontname "DejaVu Serif"))

(setf (interval *mysub*) 60)
(setf (interval-frequency *mysub*) 8)

;; Events with autoincrement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "27.60"))
(insert-event *mysub* (dialogue "Whenever sang my songs" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "ne" 90)
(insert-karaoke (last-event *mysub*) "sang" 90)

(insert-event *mysub* (dialogue "On the stage, on my own" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ge," 150)
(insert-karaoke (last-event *mysub*) "my" 90)

(insert-event *mysub* (dialogue "Whenever said my words" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ver" 90)
(insert-karaoke (last-event *mysub*) "said" 60)

(insert-event *mysub* (dialogue "Wishing they would be heard" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "they" 90)

(insert-event *mysub* (dialogue "I saw you smiling at me" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "saw" 60)
(insert-karaoke (last-event *mysub*) "ling" 90)
(insert-karaoke (last-event *mysub*) "at" 60)

(insert-event *mysub* (dialogue "Was it real or just my fantasy" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "real" 60)
(insert-karaoke (last-event *mysub*) "just" 60)
(insert-karaoke (last-event *mysub*) "my" 90)

(insert-event *mysub* (dialogue "You'd always be there in the corner" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ways" 90)

(insert-event *mysub* (dialogue "Of this tiny little bar" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 10)
(insert-karaoke (last-event *mysub*) "this" 90)
(insert-karaoke (last-event *mysub*) "ti" 180)
(increase-override (find-override (last-event *mysub*) "ny"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "1:10.40"))
(insert-event *mysub* (dialogue "My last night here for you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "night" 90)

(insert-event *mysub* (dialogue "Same old songs, just once more" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "songs," 150)

(insert-event *mysub* (dialogue "My last night here with you?" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "last" 60)
(insert-karaoke (last-event *mysub*) "night" 90)
(insert-karaoke (last-event *mysub*) "re" 30)

(insert-event *mysub* (dialogue "Maybe yes, maybe no" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "yes," 90)

(insert-event *mysub* (dialogue "I kind of liked it your way" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "kind" 90)

(insert-event *mysub* (dialogue "How you shyly placed your eyes on me" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(delete-override (last-event *mysub*) "ced")
(insert-karaoke (last-event *mysub*) "shy" 120)
(insert-karaoke (last-event *mysub*) "on" 60)

(insert-event *mysub* (dialogue "Oh, did you ever know?" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "did" 90)
(insert-karaoke (last-event *mysub*) "you" 90)

(insert-event *mysub* (dialogue "That I had mine on you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "ne" 210)
(insert-karaoke (last-event *mysub*) "on" 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "1:54.60"))
(insert-event *mysub* (dialogue "Darling, so there you are" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "re" 120)

(insert-event *mysub* (dialogue "With that look on your face" :generate-overrides-p t :spell-duration 30))

(insert-event *mysub* (dialogue "As if you're never hurt" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ne" 90)

(insert-event *mysub* (dialogue "As if you're never down" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ne" 90)

(insert-event *mysub* (dialogue "Shall I be the one for you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "I" 60)
(insert-karaoke (last-event *mysub*) "one" 120)

(insert-event *mysub* (dialogue "Who pinches you softly but sure" :generate-overrides-p t :spell-duration 30) :interval-event 11)
(insert-karaoke (last-event *mysub*) "you" 60)
(insert-karaoke (last-event *mysub*) "ly" 150)
(insert-karaoke (last-event *mysub*) "but" 90)

(insert-event *mysub* (dialogue "If frown is shown then" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 5)
(insert-karaoke (last-event *mysub*) "shown" 60)

(insert-event *mysub* (dialogue "I will know that you are no dreamer" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 16)
(delete-override (last-event *mysub*) "will")
(insert-karaoke (last-event *mysub*) "know" 90)
(insert-karaoke (last-event *mysub*) "that" 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "3:4.80"))
(insert-event *mysub* (dialogue "So let me come to you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(delete-override (last-event *mysub*) "me to")
(insert-karaoke (last-event *mysub*) "let" 90)
(insert-karaoke (last-event *mysub*) "me" 90)

(insert-event *mysub* (dialogue "Close as I wanna be" :generate-overrides-p t :spell-duration 30))
(delete-override (last-event *mysub*) "se")
(insert-karaoke (last-event *mysub*) "I" 150)

(insert-event *mysub* (dialogue "Close enough for me" :generate-overrides-p t :spell-duration 30))
(delete-override (last-event *mysub*) "se")
(insert-karaoke (last-event *mysub*) "Close" 150)
(insert-karaoke (last-event *mysub*) "enough" 90)

(insert-event *mysub* (dialogue "To feel the heart beating fast" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "heart" 120)
(insert-karaoke (last-event *mysub*) "beating" 30)

(insert-event *mysub* (dialogue "And stay there as I whisper" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "I" 90)

(insert-event *mysub* (dialogue "How I loved your peaceful eyes on me" :generate-overrides-p t :spell-duration 30))
(delete-override (last-event *mysub*) "ved")
(delete-override (last-event *mysub*) "ful")
(insert-karaoke (last-event *mysub*) "loved" 90)
(insert-karaoke (last-event *mysub*) "on" 90)

(insert-event *mysub* (dialogue "Did you ever know" :generate-overrides-p t :spell-duration 30) :interval-event 7)
(insert-karaoke (last-event *mysub*) "you" 120)

(insert-event *mysub* (dialogue "That I had mine on you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "ne" 210)
(insert-karaoke (last-event *mysub*) "on" 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "3:49.40"))
(insert-event *mysub* (dialogue "Darling, so share with me" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "re" 120)

(insert-event *mysub* (dialogue "Your love if you have enough" :generate-overrides-p t :spell-duration 30))
(delete-override (last-event *mysub*) "ve i")

(insert-event *mysub* (dialogue "Your tears if you're holding back" :generate-overrides-p t :spell-duration 30))

(insert-event *mysub* (dialogue "Or pain if that's what it is" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 9)

(insert-event *mysub* (dialogue "How can I let you know" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "let" 90)

(insert-event *mysub* (dialogue "I'm more than the dress and the voice" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 10)
(insert-karaoke (last-event *mysub*) "dress" 90)
(insert-karaoke (last-event *mysub*) "and" 90)
(insert-karaoke (last-event *mysub*) "the" 90)

(insert-event *mysub* (dialogue "Just reach me out then" :generate-overrides-p t :spell-duration 30) :interval-event 5)

(insert-event *mysub* (dialogue "You will know that you're not dreaming" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 10)
(insert-karaoke (last-event *mysub*) "know" 90)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (interval-counter *mysub*) (durationinteger "4:32.40"))
(insert-event *mysub* (dialogue "Darling, so there you are" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "re" 120)

(insert-event *mysub* (dialogue "With that look on your face" :generate-overrides-p t :spell-duration 30))

(insert-event *mysub* (dialogue "As if you're never hurt" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ne" 90)

(insert-event *mysub* (dialogue "As if you're never down" :generate-overrides-p t :spell-duration 30))
(insert-karaoke (last-event *mysub*) "ne" 90)

(insert-event *mysub* (dialogue "Shall I be the one for you" :generate-overrides-p t :spell-duration 30) :interval-delay 0)
(insert-karaoke (last-event *mysub*) "I" 60)
(insert-karaoke (last-event *mysub*) "one" 120)

(insert-event *mysub* (dialogue "Who pinches you softly but sure" :generate-overrides-p t :spell-duration 30) :interval-event 11)
(insert-karaoke (last-event *mysub*) "you" 60)
(insert-karaoke (last-event *mysub*) "ly" 150)
(insert-karaoke (last-event *mysub*) "but" 90)

(insert-event *mysub* (dialogue "If frown is shown then" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 5)
(insert-karaoke (last-event *mysub*) "shown" 60)

(insert-event *mysub* (dialogue "I will know that you are no dreamer" :generate-overrides-p t :spell-duration 30) :interval-delay 0 :interval-event 16)
(delete-override (last-event *mysub*) "will")
(insert-karaoke (last-event *mysub*) "know" 90)
(insert-karaoke (last-event *mysub*) "that" 60)

;; Disable counter
(setf (interval-counter *mysub*) nil)

(insert-style *mysub* (style "Title" :alignment 8 :shadow 2 :outline 0 :back-colour "&HA0D4FF7F" :primary-colour "&H30FFFFFF" :fontname "DejaVu San"))
(insert-style *mysub* (style "Notes" :alignment 8 :shadow 2 :outline 0 :back-colour "&HA0D4FF7F" :primary-colour "&H30C36888" :fontname "DejaVu San"))

(insert-event *mysub* (dialogue "Eyes on Me - Faye Wong" :style "Title" :start 0 :end "00:05:38"))
(insert-event *mysub* (dialogue (claraoke-version) :style "Notes" :start "00:27" :end "00:05:38"))

;; Write to file
(claraoke-subtitle-to-file *mysub* #p"/tmp/mysub.ass")


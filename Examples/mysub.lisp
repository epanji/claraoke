(ql:quickload "claraoke-user" :silent t)

(in-package :claraoke-user)

(defparameter *mysub* (subtitle "Eyes on Me" :text nil :fontsize 72 :alignment 5 :italic -1))
(defparameter *counter* 0)
(defparameter *beat* 60)
(defparameter *duration* (* 8 *beat*))

;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "27.60"))
(defparameter *line01*
  (dialogue "Whenever sang my songs"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line02*
  (dialogue "On the stage, on my own"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line03*
  (dialogue "Whenever said my words"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line04*
  (dialogue "Wishing they would be heard"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line05*
  (dialogue "I saw you smiling at me"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line06*
  (dialogue "Was it real or just my fantasy"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line07*
  (dialogue "You'd always be there in the corner of this tiny little bar"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* (* 2 *duration*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "1:10.40"))
(defparameter *line08*
  (dialogue "My last night here for you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line09*
  (dialogue "Same old songs, just once more"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line10*
  (dialogue "My last night here with you?"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line11*
  (dialogue "Maybe yes, maybe no"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line12*
  (dialogue "I kind of liked it your way"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line13*
  (dialogue "How you shyly placed your eyes on me"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line14*
  (dialogue "Oh, did you ever know?"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line15*
  (dialogue "That I had mine on you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "1:54.60"))
(defparameter *line16*
  (dialogue "Darling, so there you are"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line17*
  (dialogue "With that look on your face"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line18*
  (dialogue "As if you're never hurt"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line19*
  (dialogue "As if you're never down"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line20*
  (dialogue "Shall I be the one for you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line21*
  (dialogue "Who pinches you softly but sure"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* (+ *duration* *beat* *beat* *beat*))))
(defparameter *line22*
  (dialogue "If frown is shown then"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (- *duration* *beat* *beat* *beat*))))
(defparameter *line23*
  (dialogue "I will know that you are no dreamer"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (* 2 *duration*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "3:05"))
(defparameter *line24*
  (dialogue "So let me come to you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line25*
  (dialogue "Close as I wanna be"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line26*
  (dialogue "Close enough for me"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line27*
  (dialogue "To feel your heart beating fast"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line28*
  (dialogue "And stay there as I whisper"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line29*
  (dialogue "How I loved your peaceful eyes on me"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line30*
  (dialogue "Did you ever know"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* (- *duration* *beat*))))
(defparameter *line31*
  (dialogue "That I had mine on you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "3:49.40"))
(defparameter *line32*
  (dialogue "Darling, so share with me"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line33*
  (dialogue "Your love if you have enough"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line34*
  (dialogue "Your tears if you're holding back"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line35*
  (dialogue "Or pain if that's what it is"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (+ *duration* *beat*))))
(defparameter *line36*
  (dialogue "How can I let you know"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line37*
  (dialogue "I'm more than the dress and the voice"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (+ *duration* *beat* *beat*))))
(defparameter *line38*
  (dialogue "Just reach me out then"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* (- *duration* *beat* *beat* *beat*))))
(defparameter *line39*
  (dialogue "You will know that you're not dreaming"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (+ *duration* *beat* *beat*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *counter* (durationinteger "4:32.40"))
(defparameter *line40*
  (dialogue "Darling, so there you are"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line41*
  (dialogue "With that look on your face"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line42*
  (dialogue "As if you're never hurt"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line43*
  (dialogue "As if you're never down"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* *duration*)))
(defparameter *line44*
  (dialogue "Shall I be the one for you"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* *duration*)))
(defparameter *line45*
  (dialogue "Who pinches you softly but sure"
            :generate-overrides-p t
            :spell-duration 30
            :start (incf *counter* *beat*)
            :end (incf *counter* (+ *duration* *beat* *beat* *beat*))))
(defparameter *line46*
  (dialogue "If frown is shown then"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (- *duration* *beat* *beat* *beat*))))
(defparameter *line47*
  (dialogue "I will know that you are no dreamer"
            :generate-overrides-p t
            :spell-duration 30
            :start *counter*
            :end (incf *counter* (* 2 *duration*))))

;; Overrides
;; TODO
;; (defun k (d) (format nil "K~D" d))
;; (setf (overrides *line01*)
;;       (list (override (k *beat*) 0)
;;             (override (k *beat*) 4)
;;             (override (k *beat*) 9)
;;             (override (k *beat*) 14)
;;             (override (k *beat*) 17)))
;; (setf (overrides *line02*)
;;       (list (override (k *beat*) 0)
;;             (override (k *beat*) 3)
;;             (override (k (* 2 *beat*)) 7)
;;             (override (k *beat*) 17)
;;             (override (k *beat*) 20)))

;; Add lines
(insert-event *mysub* *line01*)
(insert-event *mysub* *line02*)
(insert-event *mysub* *line03*)
(insert-event *mysub* *line04*)
(insert-event *mysub* *line05*)
(insert-event *mysub* *line06*)
(insert-event *mysub* *line07*)
(insert-event *mysub* *line08*)
(insert-event *mysub* *line09*)
(insert-event *mysub* *line10*)
(insert-event *mysub* *line11*)
(insert-event *mysub* *line12*)
(insert-event *mysub* *line13*)
(insert-event *mysub* *line14*)
(insert-event *mysub* *line15*)
(insert-event *mysub* *line16*)
(insert-event *mysub* *line17*)
(insert-event *mysub* *line18*)
(insert-event *mysub* *line19*)
(insert-event *mysub* *line20*)
(insert-event *mysub* *line21*)
(insert-event *mysub* *line22*)
(insert-event *mysub* *line23*)
(insert-event *mysub* *line24*)
(insert-event *mysub* *line25*)
(insert-event *mysub* *line26*)
(insert-event *mysub* *line27*)
(insert-event *mysub* *line28*)
(insert-event *mysub* *line29*)
(insert-event *mysub* *line30*)
(insert-event *mysub* *line31*)
(insert-event *mysub* *line32*)
(insert-event *mysub* *line33*)
(insert-event *mysub* *line34*)
(insert-event *mysub* *line35*)
(insert-event *mysub* *line36*)
(insert-event *mysub* *line37*)
(insert-event *mysub* *line38*)
(insert-event *mysub* *line39*)
(insert-event *mysub* *line40*)
(insert-event *mysub* *line41*)
(insert-event *mysub* *line42*)
(insert-event *mysub* *line43*)
(insert-event *mysub* *line44*)
(insert-event *mysub* *line45*)
(insert-event *mysub* *line46*)
(insert-event *mysub* *line47*)

;; Write to file
(claraoke-subtitle-to-file *mysub* #p"/tmp/mysub.ass")


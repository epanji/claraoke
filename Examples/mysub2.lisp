(ql:quickload "claraoke-user" :silent t)

(in-package :claraoke-user)

(defparameter *mysub2* (subtitle "Background" :text nil))

;; Create 5000 dialogue
(dotimes (i 5000)
  (insert-event *mysub2* (dialogue "m 0 0 l 100 30 130 130 160 30 260 0 160 -30 130 -130 100 -30"
                                   :start (random (durationinteger "00:00:30.00")) :duration 100
                                   :overrides (list (override 'drawing-mode 0
                                                              :arg1 4
                                                              :modifiers
                                                              (list
                                                               (modifier 'fontrotate-z :arg1 (- 0 (random 90)))
                                                               (modifier 'border :arg1 10)
                                                               (modifier 'blur-edges :arg1 20)
                                                               (modifier 'pos :arg1 (random 1280) :arg2 (random 720))
                                                               (modifier 'fad :arg1 500 :arg2 500)
                                                               (modifier 'color1 :arg1 (colorstring (random-color 1/5)))
                                                               (modifier 'color3 :arg1 (colorstring (random-color 3/5)))))))))

;; Write to file
(claraoke-subtitle-to-file *mysub2* #p"/tmp/mysub2.ass")


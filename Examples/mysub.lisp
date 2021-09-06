(in-package #:claraoke-user)

(defparameter *mysub* (subtitle "mysub"))

(defparameter *mydia* (last-event *mysub*))

(setf (text *mydia*) (text "berjalan di hutan cemara" :generate-overrides-p t)
      (start *mydia*) "5.00"
      (duration-length *mydia*) "2.00")

(insert-event *mysub* (dialogue "langkahku terasa kecil dan lelah"
                                :start "8.00" :duration "5.00"
                                :generate-overrides-p t))

(insert-event *mysub* (dialogue "makin dalam lagi"
                                :start "14.00"
                                :end "16.00"
                                :generate-overrides-p t))

(insert-event *mysub* (dialogue "ku ditelan fatamorgana"
                                :start "17.00"
                                :end "23.00"
                                :generate-overrides-p t))

(claraoke-subtitle-to-file *mysub* #p"/tmp/mysub.ass" :if-exists :supersede)
(claraoke-subtitle-dummy-video *mysub* #p"/tmp/mysub.mp4")


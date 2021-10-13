(cl:in-package #:cl-user)

(defpackage #:claraoke-user
  (:use #:common-lisp #:claraoke)
  (:export
   #:claraoke-subtitle-dummy-video
   #:claraoke-subtitle-to-file
   #:claraoke-version))

(cl:in-package #:claraoke-user)

(defun claraoke-version ()
  (claraoke-internal:version))

(defun claraoke-subtitle-to-file (object filespec &key (if-exists :rename) (if-does-not-exist :create))
  (check-type filespec pathname)
  (with-open-file (stream filespec :direction :output
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)
    (print-script object stream)))

(defun claraoke-subtitle-dummy-video (object filespec &optional (color "#008000"))
  (check-type filespec pathname)
  (if (probe-file filespec)
      (format t "File ~A already exists." (namestring filespec))
      (let ((max (apply 'max (loop for event in (lines (events object))
                                   collect (durationinteger (end event)))))
            (width (value (find-info (script-info object) "PlayResX")))
            (height (value (find-info (script-info object) "PlayResY"))))
        (uiop:run-program (format nil "ffmpeg -lavfi color=c=~A:s=~Dx~D -t ~D ~S"
                                  color
                                  width
                                  height
                                  (+ 5 (ceiling max 100))
                                  (namestring filespec))
                          :force-shell t)))
  filespec)


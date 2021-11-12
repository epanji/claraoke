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
      (let* ((max (apply 'max (loop for event in (lines (events object))
                                    collect (durationinteger (end event)))))
             (width (value (find-info object "PlayResX")))
             (height (value (find-info object "PlayResY")))
             (ffmpeg (format nil "ffmpeg -lavfi color=c=~A:s=~Dx~D -t ~D ~S"
                             color
                             width
                             height
                             (ceiling max 100)
                             (namestring filespec))))
        (uiop:run-program ffmpeg :force-shell t)))
  filespec)

(defun claraoke-subtitle-hardsub-video (subtitle &optional video-or-color)
  (check-type subtitle pathname)
  (check-type video-or-color (or null pathname string))
  (let* ((object (parse-script subtitle))
         (max (apply 'max (loop for event in (lines (events object))
                                collect (durationinteger (end event)))))
         (width (value (find-info object "PlayResX")))
         (height (value (find-info object "PlayResY")))
         (input (if (pathnamep video-or-color)
                    (format nil "-i ~S -lavfi ass=~S" (namestring video-or-color) (namestring subtitle))
                    (format nil "-lavfi color=c=~A:s=~Dx~D,ass=~S -t ~D"
                            (or video-or-color "#008000")
                            width height (namestring subtitle) (ceiling max 100))))
         (output (translate-pathname subtitle "*.*" "hardsub.mp4")))
    (if (probe-file output)
        (format t "File ~A already exists." (namestring output))
        (uiop:run-program (format nil "ffmpeg ~A ~S" input (namestring output)) :force-shell t))))


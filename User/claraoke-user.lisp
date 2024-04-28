(cl:in-package #:claraoke-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This system purpose just for example when using CLARAOKE system.
;;;; It is designed with minimum dependencies for user experiences.
;;;; Some functions only execute on unix/like operating system and
;;;; only return command string for others.
;;;;

(defun claraoke-version ()
  (claraoke-internal:version))

(defun claraoke-subtitle-to-file (object filespec &key (if-exists :rename) (if-does-not-exist :create))
  "Write OBJECT argument to FILESPEC pathname. It will create new file
if possible or rename existing file by default."
  (check-type filespec pathname)
  (with-open-file (stream filespec :direction :output
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)
    (print-script object stream)))

(defun execute-command (string
                        &key
                          (cmd "/bin/sh")
                          (opt "-c")
                          (io nil)
                          (wt t)
                        &aux (argv (list opt string)))
  (let ((exit-code #+(and unix sbcl) (sb-ext:process-exit-code (sb-ext:run-program cmd argv :input io :output io :wait wt))
                   #+(and unix clisp) (or (ext:run-program cmd :arguments argv :input io :output io :wait wt) 0)
                   #+(and unix ecl) (nth-value 1 (ext:run-program cmd argv :input io :output io :wait wt))
                   #+(and unix abcl) (sys:process-exit-code (sys:run-program cmd argv :input io :output io :wait wt))
                   #-(and unix (or sbcl clisp ecl abcl)) 1))
    (if (zerop exit-code) exit-code string)))

(defun html-color-code (input)
  (let ((c (color input)))
    (format nil "#~2,'0X~2,'0X~2,'0X" (red c) (green c) (blue c))))

(defun claraoke-subtitle-dummy-video (object filespec &optional (color "#008000"))
  (check-type filespec pathname)
  (if (probe-file filespec)
      (format t "~&File ~A already exists.~%" (namestring filespec))
      (let* ((max (apply 'max (loop for event in (lines (events object))
                                    collect (durationinteger (end event)))))
             (ffmpeg (format nil "ffmpeg -lavfi color=c=~A:s=~Dx~D -t ~D ~S"
                             (html-color-code color)
                             (value (find-info object "PlayResX"))
                             (value (find-info object "PlayResY"))
                             (ceiling max 100)
                             (namestring filespec))))
        (if (stringp (execute-command ffmpeg))
            ffmpeg
            filespec))))

(defun claraoke-subtitle-hardsub-video (subtitle &optional (video-or-color "#008000"))
  (check-type subtitle pathname)
  (let* ((object (parse-script subtitle))
         (max (apply 'max (loop for event in (lines (events object))
                                collect (durationinteger (end event)))))
         (input (if (pathnamep video-or-color)
                    (format nil "-i ~S -lavfi ass=~S" (namestring video-or-color) (namestring subtitle))
                    (format nil "-lavfi color=c=~A:s=~Dx~D,ass=~S -t ~D"
                            (html-color-code video-or-color)
                            (value (find-info object "PlayResX"))
                            (value (find-info object "PlayResY"))
                            (namestring subtitle)
                            (ceiling max 100))))
         (output (merge-pathnames (format nil "~A-hardsub.mp4" (pathname-name subtitle)) subtitle)))
    (if (probe-file output)
        (format t "~&File ~A already exists.~%" (namestring output))
        (let ((ffmpeg (format nil "ffmpeg ~A ~S" input (namestring output))))
          (if (stringp (execute-command ffmpeg))
              ffmpeg
              output)))))


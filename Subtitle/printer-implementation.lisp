(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script (ASS)
;;;
(defmethod claraoke:print-script ((object note) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (princ #\; stream)
    (princ #\Space stream)
    (princ (claraoke:descriptor object) stream)
    (terpri stream))
  object)

(defmethod claraoke:print-script ((object info) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (princ (claraoke:descriptor object) stream)
    (princ #\: stream)
    (princ #\Space stream)
    (princ (claraoke:value object) stream)
    (terpri stream))
  object)

(defmethod claraoke:print-script ((object subtitle) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (claraoke:print-script (claraoke:lines object) stream))
  object)

(defmethod claraoke:print-script ((object cons) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for line in object
          do (claraoke:print-script line stream)
          finally (terpri stream)))
  object)

(defmethod claraoke:print-script ((object vector) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for line across object
          do (claraoke:print-script line stream)))
  object)

(defmethod claraoke:print-script ((object section) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream))
        (header (claraoke:header object))
        (lines (claraoke:lines object)))
    (unless (null lines)
      (princ #\[ stream)
      (princ (claraoke:descriptor object) stream)
      (princ #\] stream)
      (terpri stream)
      (unless (null header)
        (claraoke:print-script header stream)
        (terpri stream))
      (claraoke:print-script (reverse lines) stream)))
  object)

(defmethod claraoke:print-script ((object style) &optional stream)
  (let ((slots (list (claraoke:name object)
                     (claraoke:fontname object)
                     (claraoke:fontsize object)
                     (claraoke:primary-colour object)
                     (claraoke:secondary-colour object)
                     (claraoke:outline-colour object)
                     (claraoke:back-colour object)
                     (claraoke:bold object)
                     (claraoke:italic object)
                     (claraoke:underline object)
                     (claraoke:strike-out object)
                     (claraoke:scale-x object)
                     (claraoke:scale-y object)
                     (claraoke:spacing object)
                     (claraoke:angle object)
                     (claraoke:border-style object)
                     (claraoke:outline object)
                     (claraoke:.shadow object)
                     (claraoke:alignment object)
                     (claraoke:margin-l object)
                     (claraoke:margin-r object)
                     (claraoke:margin-v object)
                     (claraoke:encoding object)))
        (separator (claraoke:separator object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (princ (claraoke:descriptor object) stream)
    (princ #\: stream)
    (princ #\Space stream)
    (princ (first slots) stream)
    (loop for slot in (rest slots)
          do (princ separator stream)
             (claraoke:print-script slot stream))
    (terpri stream))
  object)

(defmethod claraoke:print-script ((object event) &optional stream)
  (let ((slots (list (claraoke:layer object)
                     (claraoke:start object)
                     (claraoke:end object)
                     (claraoke:.style object)
                     (claraoke:name object)
                     (claraoke:margin-l object)
                     (claraoke:margin-r object)
                     (claraoke:margin-v object)
                     (claraoke:effect object)
                     (claraoke:.text object)))
        (separator (claraoke:separator object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (princ (claraoke:descriptor object) stream)
    (princ #\: stream)
    (princ #\Space stream)
    (princ (first slots) stream)
    (loop for slot in (rest slots)
          do (princ separator stream)
             (claraoke:print-script slot stream))
    (terpri stream))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print object
;;;
(defmethod print-object ((object note) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ "Comment" stream)
    (princ #\Space stream)
    (prin1 (claraoke:descriptor object) stream)))

(defmethod print-object ((object info) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ (claraoke:descriptor object) stream)
    (princ #\Space stream)
    (prin1 (claraoke:value object) stream)))

(defmethod print-object ((object subtitle) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (let* ((info (claraoke:find-line (aref (claraoke:lines object) 0) "Title"))
           (title (if (null info)
                      "Untitled"
                      (claraoke:value info))))
      (princ "SUBTITLE" stream)
      (princ #\Space stream)
      (prin1 title stream))))

(defmethod print-object ((object style) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ "STYLE" stream)
    (princ #\Space stream)
    (prin1 (claraoke:name object) stream)))

(defmethod print-object ((object event) stream)
  (let ((start (claraoke:start object))
        (end (claraoke:end object))
        (text (claraoke:.text object)))
    (format stream "#<~A --> ~A ~S ~@[(~D)>~]" start end
            (if (stringp text) text (claraoke:.text text))
            (unless (stringp text)
              (length (claraoke:overrides text))))))


(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtitle
;;;
(defclass subtitle ()
  ((%script-info
    :initform nil
    :initarg :script-info
    :accessor claraoke:.script-info)
   (%styles
    :initform '()
    :initarg :styles
    :accessor claraoke:styles)
   (%events
    :initform '()
    :initarg :events
    :accessor claraoke:events)))

(defmethod initialize-instance :after ((instance subtitle) &rest initargs &key &allow-other-keys)
  (let ((args (append initargs (list :allow-other-keys t))))
    ;; Use more specific keys
    (remf args :name)
    (remf args :margin-l)
    (remf args :margin-r)
    (remf args :margin-v)
    ;; Apply modified args
    (setf (claraoke:.script-info instance)
          (apply 'make-instance 'script-info args))
    (setf (claraoke:styles instance)
          (list (apply 'make-instance 'style args)))
    (setf (claraoke:events instance)
          (list (apply 'make-instance 'dialogue args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script info
;;;
(defclass script-info ()
  ((%title
    :initform "ASS file"
    :initarg :title
    :accessor claraoke:title)
   (%script-type
    :initform "v4.00+"
    :initarg :script-type
    :accessor claraoke:script-type)
   (%wrap-style
    :initform 0
    :initarg :wrap-style
    :accessor claraoke:wrap-style)
   (%play-res-x
    :initform 1366
    :initarg :play-res-x
    :accessor claraoke:play-res-x)
   (%play-res-y
    :initform 768
    :initarg :play-res-y
    :accessor claraoke:play-res-y)
   (%scaled-border-and-shadow
    :initform "yes"
    :initarg :scaled-border-and-shadow
    :accessor claraoke:scaled-border-and-shadow)
   (%last-style-storage
    :initform nil                       ; optional
    :initarg :last-style-storage
    :accessor claraoke:last-style-storage)
   (%video-aspect-ratio
    :initform 0
    :initarg :video-aspect-ratio
    :accessor claraoke:video-aspect-ratio)
   (%video-zoom
    :initform 0
    :initarg :video-zoom
    :accessor claraoke:video-zoom)
   (%video-position
    :initform 0
    :initarg :video-position
    :accessor claraoke:video-position)
   (%original-translation
    :initform nil                       ; optional
    :initarg :original-translation
    :accessor claraoke:original-translation)
   (%collisions
    :initform "Normal"
    :initarg :collisions
    :accessor claraoke:collisions)))

(macrolet ((subtitle-specializer (name)
             `(progn (defmethod ,name ((object subtitle))
                       (,name (claraoke:.script-info object)))
                     (defmethod (setf ,name) (new-value (object subtitle))
                       (setf (,name (claraoke:.script-info object)) new-value)))))
  (subtitle-specializer claraoke:title)
  (subtitle-specializer claraoke:script-type)
  (subtitle-specializer claraoke:wrap-style)
  (subtitle-specializer claraoke:play-res-x)
  (subtitle-specializer claraoke:play-res-y)
  (subtitle-specializer claraoke:scaled-border-and-shadow)
  (subtitle-specializer claraoke:last-style-storage)
  (subtitle-specializer claraoke:video-aspect-ratio)
  (subtitle-specializer claraoke:video-zoom)
  (subtitle-specializer claraoke:video-position)
  (subtitle-specializer claraoke:original-translation)
  (subtitle-specializer claraoke:collisions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style
;;;
(defclass style ()
  ((%name
    :initform "Default"
    :initarg :name
    :initarg :style-name
    :accessor claraoke:name)
   (%fontname
    :initform "Arial"
    :initarg :fontname
    :accessor claraoke:fontname)
   (%fontsize
    :initform 48
    :initarg :fontsize
    :accessor claraoke:fontsize)
   (%primary-colour
    :initform "&H00FFFFFF"
    :initarg :primary-colour
    :accessor claraoke:primary-colour)
   (%secondary-colour
    :initform "&H000000FF"
    :initarg :secondary-colour
    :accessor claraoke:secondary-colour)
   (%outline-colour
    :initform "&H00000000"
    :initarg :outline-colour
    :accessor claraoke:outline-colour)
   (%back-colour
    :initform "&H00000000"
    :initarg :back-colour
    :accessor claraoke:back-colour)
   (%bold
    :initform 0
    :initarg :bold
    :accessor claraoke:bold)
   (%italic
    :initform 0
    :initarg :italic
    :accessor claraoke:italic)
   (%underline
    :initform 0
    :initarg :underline
    :accessor claraoke:underline)
   (%strike-out
    :initform 0
    :initarg :strike-out
    :accessor claraoke:strike-out)
   (%scale-x
    :initform 100
    :initarg :scale-x
    :accessor claraoke:scale-x)
   (%scale-y
    :initform 100
    :initarg :scale-y
    :accessor claraoke:scale-y)
   (%spacing
    :initform 0
    :initarg :spacing
    :accessor claraoke:spacing)
   (%angle
    :initform 0
    :initarg :angle
    :accessor claraoke:angle)
   (%border-style
    :initform 1
    :initarg :border-style
    :accessor claraoke:border-style)
   (%outline
    :initform 2
    :initarg :outline
    :accessor claraoke:outline)
   (%shadow
    :initform 0
    :initarg :shadow
    :accessor claraoke:.shadow)
   (%alignment
    :initform 8
    :initarg :alignment
    :accessor claraoke:alignment)
   (%margin-l
    :initform 25
    :initarg :margin-l
    :initarg :style-margin-l
    :accessor claraoke:margin-l)
   (%margin-r
    :initform 25
    :initarg :margin-r
    :initarg :style-margin-r
    :accessor claraoke:margin-r)
   (%margin-v
    :initform 25
    :initarg :margin-v
    :initarg :style-margin-v
    :accessor claraoke:margin-v)
   (%encoding
    :initform 1
    :initarg :encoding
    :accessor claraoke:encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event
;;;
(defclass event ()
  ((%layer
    :initarg :layer
    :accessor claraoke:layer
    :initform 0)
   (%start
    :initarg :start
    :accessor claraoke:start
    :initform "00:00:00.00")
   (%end
    :initarg :end
    :initarg :duration
    :accessor claraoke:end
    :initform "00:00:03.00")
   (%style
    :initarg :style
    :accessor claraoke:.style
    :initform "Default")
   (%name
    :initarg :name
    :initarg :event-name
    :accessor claraoke:name
    :initform "")
   (%margin-l
    :initarg :margin-l
    :initarg :event-margin-l
    :accessor claraoke:margin-l
    :initform 0)
   (%margin-r
    :initarg :margin-r
    :initarg :event-margin-r
    :accessor claraoke:margin-r
    :initform 0)
   (%margin-v
    :initarg :margin-v
    :initarg :event-margin-v
    :accessor claraoke:margin-v
    :initform 0)
   (%effect
    :initarg :effect
    :accessor claraoke:effect
    :initform "")
   (%text
    :initarg :text
    :accessor claraoke:.text
    :initform "")))

(defmethod print-object ((event event) stream)
  (let ((start (claraoke:start event))
        (end (claraoke:end event))
        (text (claraoke:.text event)))
    (format stream "~A --> ~A ~S" start end text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialogue, comment, picture, sound, movie and
;;; command class with default initargs.
;;;
(defclass dialogue (event)
  ()
  (:default-initargs
   :text "Text Here"))

(defclass comment (event)
  ()
  (:default-initargs
   :text "This is comment"))

(defclass picture (event)
  ()
  (:default-initargs
   :text "/path/to/the/file.jpg"))

(defclass sound (event)
  ()
  (:default-initargs
   :text "/path/to/the/file.wav"))

(defclass movie (event)
  ()
  (:default-initargs
   :text "/path/to/the/file.avi"))

(defclass command (event)
  ()
  (:default-initargs
   :text "SSA:pause"))


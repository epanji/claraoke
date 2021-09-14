(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line
;;;
(defclass note (claraoke:line)
  ()
  (:default-initargs
   :descriptor "This is a comment."))

(defclass info (claraoke:line claraoke:value-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtitle have 5 ordered sections
;;;
;;; 1. Script info
;;; 2. V4+ Styles
;;; 3. Events
;;; 4. Fonts
;;; 5. Graphics
;;;
(defclass subtitle (claraoke:script)
  ()
  (:default-initargs
   :lines (make-array 5 :initial-element nil))
  (:documentation "#(script-info styles events fonts graphics)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sections
;;;
(defclass section (claraoke:section-line)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script info
;;;
(defclass script-info (section)
  ()
  (:default-initargs
   :descriptor "Script Info"
   :lines ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Styles
;;;
(defclass style (claraoke:table-line)
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
    :initform 32
    :initarg :fontsize
    :accessor claraoke:fontsize)
   (%primary-colour
    :initform "&H0000A5FF"
    :initarg :primary-colour
    :accessor claraoke:primary-colour)
   (%secondary-colour
    :initform "&H00FFFFFF"
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
    :initform 1
    :initarg :outline
    :accessor claraoke:outline)
   (%shadow
    :initform 1
    :initarg :shadow
    :accessor claraoke:.shadow)
   (%alignment
    :initform 2
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
    :initform 72
    :initarg :margin-v
    :initarg :style-margin-v
    :accessor claraoke:margin-v)
   (%encoding
    :initform 1
    :initarg :encoding
    :accessor claraoke:encoding))
  (:default-initargs
   :separator #\,
   :descriptor "Style"))

(defclass styles (section)
  ()
  (:default-initargs
   :descriptor "V4+ Styles"
   :header (format nil "~
    Format: Name, Fontname, Fontsize, ~
    PrimaryColour, SecondaryColour, OutlineColour, BackColour, ~
    Bold, Italic, Underline, StrikeOut, ~
    ScaleX, ScaleY, Spacing, Angle, ~
    BorderStyle, Outline, Shadow, Alignment, ~
    MarginL, MarginR, MarginV, Encoding")
   :lines ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Events
;;;
;;; Dialogue, Comment, Picture, Sound, Movie and
;;; Command
;;;
(defclass event (claraoke:table-line)
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
    :initform ""))
  (:default-initargs
   :separator #\,))

(defclass dialogue (event)
  ()
  (:default-initargs
   :descriptor "Dialogue"
   :text "Text Here"))

(defclass comment (event)
  ()
  (:default-initargs
   :descriptor "Comment"
   :text "This is comment"))

(defclass picture (event)
  ()
  (:default-initargs
   :descriptor "Picture"
   :text "/path/to/the/file.jpg"))

(defclass sound (event)
  ()
  (:default-initargs
   :descriptor "Sound"
   :text "/path/to/the/file.wav"))

(defclass movie (event)
  ()
  (:default-initargs
   :descriptor "Movie"
   :text "/path/to/the/file.avi"))

(defclass command (event)
  ()
  (:default-initargs
   :descriptor "Command"
   :text "SSA:pause"))

(defclass events (section)
  ()
  (:default-initargs
   :descriptor "Events"
   :header (format nil "~
    Format: Layer, Start, End, Style, Name, ~
    MarginL, MarginR, MarginV, Effect, Text")
   :lines ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fonts
;;;
(defclass fonts (section)
  ()
  (:default-initargs
   :descriptor "Fonts"
   :lines ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphics
;;;
(defclass graphics (section)
  ()
  (:default-initargs
   :descriptor "Graphics"
   :lines ()))


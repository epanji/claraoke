(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;
(defclass text ()
  ((%original-text
    :initform nil
    :initarg :original-text
    :accessor claraoke:original-text)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:.text)
   (%overrides
    :initform '()
    :initarg :overrides
    :accessor claraoke:overrides)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defclass modifier ()
  ((%format-control
    :initform nil
    :initarg :format-control
    :accessor format-control)
   (arg1 :initarg :arg1 :initform nil)
   (arg2 :initarg :arg2 :initform nil)
   (arg3 :initarg :arg3 :initform nil)
   (arg4 :initarg :arg4 :initform nil)
   (arg5 :initarg :arg5 :initform nil)
   (arg6 :initarg :arg5 :initform nil)
   (arg7 :initarg :arg6 :initform nil))
  (:documentation "Modifier style, function, drawing"))

(defclass override ()
  ((%index
    :initform nil
    :initarg :index
    :accessor claraoke:index)))

(defclass batch (override)
  ((%overrides
    :initform ()
    :initarg :overrides
    :accessor claraoke:overrides)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style modifiers
;;;
(defclass newline (override modifier)
  ()
  (:default-initargs
   :format-control "\\~:[n~;N~]"
   :arg1 nil))

(defclass unknown (modifier)
  ()
  (:default-initargs
   :format-control "\\~A"
   :arg1 "Any"))

(defclass bold (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\b~D~]"
   :arg1 1))

(defclass italic (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\i~D~]"
   :arg1 1))

(defclass underline (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\u~D~]"
   :arg1 1))

(defclass strikeout (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\s~D~]"
   :arg1 1))

(defclass border (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\bord~D~]"
   :arg1 1))

(defclass .shadow (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\shad~D~]"
   :arg1 1))

(defclass blur-edges (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\be~D~]"
   :arg1 1))

(defclass fontname (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fn~A~]"
   :arg1 "Arial"))

(defclass fontsize (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fs~D~]"
   :arg1 36))

(defclass fontscale (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fsc~A~]~@[~D~]"
   :arg1 #\x ; or y
   :arg2 100))

(defclass fontspace (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fsp~D~]"
   :arg1 0))

(defclass fontrotate (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fr~A~]~@[~D~]"
   :arg1 #\x ; or y or z
   :arg2 30))

(defclass fontencoding (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fe~D~]"
   :arg1 1))

(defclass color (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\c~A~]"
   :arg1 "&HFF0011&"))

(defclass color1 (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\1c~A~]"
   :arg1 "&HFF0011&"))

(defclass color2 (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\2c~A~]"
   :arg1 "&HFF0011&"))

(defclass color3 (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\3c~A~]"
   :arg1 "&HFF0011&"))

(defclass color4 (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\4c~A~]"
   :arg1 "&HFF0011&"))

(defclass alignment (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\a~D~]"
   :arg1 2))

(defclass alignment-numpad (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\an~D~]"
   :arg1 8))

(defclass karaoke (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\k~D~]"
   :arg1 15))

(defclass karaoke-fill (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\kf~D~]"
   :arg1 15))

(defclass karaoke-outline (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\ko~D~]"
   :arg1 15))

(defclass wrapping-style (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\q~D~]"
   :arg1 1))

(defclass reset (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\r~A~]"
   :arg1 "Default"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function modifiers
;;;
(defclass transformation (modifier)
  ()
  (:default-initargs
   ;; Too complicated to split arguments due to optional position and
   ;; lack of keywords
   :format-control "~@[\\t(~A)~]"
   :arg1 "0,1000,1,\\fr30"))

(defclass move (modifier)
  ()
  (:default-initargs
   :format-control "\\move(~A,~A,~A,~A~@[,~A~]~@[,~A~])"
   :arg1 0
   :arg2 0
   :arg3 100
   :arg4 100
   :arg5 nil
   :arg6 nil))

(defclass pos (modifier)
  ()
  (:default-initargs
   :format-control "\\pos(~A,~A)"
   :arg1 10
   :arg2 10))

(defclass origin (modifier)
  ()
  (:default-initargs
   :format-control "\\org(~A,~A)"
   :arg1 10
   :arg2 10))

(defclass fade (modifier)
  ()
  (:default-initargs
   :format-control "\\fade(~A,~A,~A,~A,~A,~A,~A)"
   :arg1 10
   :arg2 10
   :arg3 10
   :arg4 10
   :arg5 10
   :arg6 10
   :arg7 10))

(defclass fad (modifier)
  ()
  (:default-initargs
   :format-control "\\fad(~A,~A)"
   :arg1 10
   :arg2 10))

(defclass clip-rectangle (modifier)
  ()
  (:default-initargs
   :format-control "\\clip(~A,~A,~A,~A)"
   :arg1 1
   :arg2 1
   :arg3 100
   :arg4 100))

(defclass clip-drawing (modifier)
  ()
  (:default-initargs
   :format-control "\\clip(~A)"
   ;; Too complicated to split arguments due to optional position and
   ;; lack of keywords
   :arg1 "1,m 0 0 l 100 0 100 100 0 100"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing modifiers
;;;
(defclass drawing-mode (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\p~D~]"
   ;; 1 on 0 off
   :arg1 1))

(defclass drawing-baseline-offset (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\pbo~D~]"
   ;; up < 0, down > 0
   :arg1 1))


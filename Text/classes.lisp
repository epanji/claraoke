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
   (%unique-p
    :initform t
    :initarg :unique-p
    :reader unique-p)
   (%arg1 :initarg :arg1 :initform nil :accessor claraoke:arg1)
   (%arg2 :initarg :arg2 :initform nil :accessor claraoke:arg2)
   (%arg3 :initarg :arg3 :initform nil :accessor claraoke:arg3)
   (%arg4 :initarg :arg4 :initform nil :accessor claraoke:arg4)
   (%arg5 :initarg :arg5 :initform nil :accessor claraoke:arg5)
   (%arg6 :initarg :arg6 :initform nil :accessor claraoke:arg6)
   (%arg7 :initarg :arg7 :initform nil :accessor claraoke:arg7))
  (:documentation "Modifier style, function, drawing"))

(defclass override ()
  ((%index
    :initform nil
    :initarg :index
    :accessor claraoke:index)))

(defclass batch (override)
  ((%modifiers
    :initform '()
    :initarg :modifiers
    :accessor claraoke:modifiers)))

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
   :unique-p nil
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

(defclass fontscale-x (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fscx~D~]"
   :arg1 100.1))

(defclass fontscale-y (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fscy~D~]"
   :arg1 100.1))

(defclass fontspace (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fsp~D~]"
   :arg1 0))

(defclass fontrotate (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fr~D~]"
   :arg1 30))

(defclass fontrotate-x (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\frx~D~]"
   :arg1 30))

(defclass fontrotate-y (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fry~D~]"
   :arg1 30))

(defclass fontrotate-z (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\frz~D~]"
   :arg1 30))

(defclass fontencoding (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fe~D~]"
   :arg1 1))

(defclass color (modifier)
  ()
  (:default-initargs
   :format-control "\\c~@[~A~]"
   :arg1 "&HFF0011&"))

(defclass color1 (modifier)
  ()
  (:default-initargs
   :format-control "\\1c~@[~A~]"
   :arg1 "&HFF0011&"))

(defclass color2 (modifier)
  ()
  (:default-initargs
   :format-control "\\2c~@[~A~]"
   :arg1 "&HFF0011&"))

(defclass color3 (modifier)
  ()
  (:default-initargs
   :format-control "\\3c~@[~A~]"
   :arg1 "&HFF0011&"))

(defclass color4 (modifier)
  ()
  (:default-initargs
   :format-control "\\4c~@[~A~]"
   :arg1 "&HFF0011&"))

(defclass alpha (modifier)
  ()
  (:default-initargs
   :format-control "\\alpha~@[~A~]"
   :arg1 "&H00&"))

(defclass alpha1 (modifier)
  ()
  (:default-initargs
   :format-control "\\1a~@[~A~]"
   :arg1 "&H00&"))

(defclass alpha2 (modifier)
  ()
  (:default-initargs
   :format-control "\\2a~@[~A~]"
   :arg1 "&H00&"))

(defclass alpha3 (modifier)
  ()
  (:default-initargs
   :format-control "\\3a~@[~A~]"
   :arg1 "&H00&"))

(defclass alpha4 (modifier)
  ()
  (:default-initargs
   :format-control "\\4a~@[~A~]"
   :arg1 "&H00&"))

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

(defclass karaoke-capital (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\K~D~]"
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
   :format-control "\\r~@[~A~]"
   :arg1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style modifiers extended
;;;
(defclass blur (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\blur~D~]"
   :arg1 1))

(defclass fontshear-x (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fax~D~]"
   :arg1 1))

(defclass fontshear-y (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\fay~D~]"
   :arg1 1))

(defclass border-x (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\xbord~D~]"
   :arg1 1))

(defclass border-y (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\ybord~D~]"
   :arg1 1))

(defclass shadow-x (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\xshad~D~]"
   :arg1 1))

(defclass shadow-y (modifier)
  ()
  (:default-initargs
   :format-control "~@[\\yshad~D~]"
   :arg1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function modifiers
;;;
(defclass transformation1 (modifier)
  ()
  (:default-initargs
   :unique-p nil
   :format-control "~@[\\t(~A)~]"
   :arg1 "\\frz30"))

(defclass transformation2 (modifier)
  ()
  (:default-initargs
   :unique-p nil
   :format-control "~@[\\t(~A,~A)~]"
   :arg1 0.5
   :arg2 "\\frz30"))

(defclass transformation3 (modifier)
  ()
  (:default-initargs
   :unique-p nil
   :format-control "~@[\\t(~A,~A,~A)~]"
   :arg1 300
   :arg2 400
   :arg3 "\\frz30"))

(defclass transformation4 (modifier)
  ()
  (:default-initargs
   :unique-p nil
   :format-control "~@[\\t(~A,~A,~A,~A)~]"
   :arg1 300
   :arg2 400
   :arg3 0.5
   :arg4 "\\frz30"))

(defclass move (modifier)
  ()
  (:default-initargs
   :format-control "\\move(~A,~A,~A,~A~@[,~A~]~@[,~A~])"
   :arg1 100
   :arg2 150
   :arg3 300
   :arg4 350
   :arg5 nil
   :arg6 nil))

(defclass pos (modifier)
  ()
  (:default-initargs
   :format-control "\\pos(~A,~A)"
   :arg1 320
   :arg2 240))

(defclass origin (modifier)
  ()
  (:default-initargs
   :format-control "\\org(~A,~A)"
   :arg1 320
   :arg2 240))

(defclass fade (modifier)
  ()
  (:default-initargs
   :format-control "\\fade(~A,~A,~A,~A,~A,~A,~A)"
   :arg1 255
   :arg2 32
   :arg3 224
   :arg4 0
   :arg5 500
   :arg6 2000
   :arg7 2200))

(defclass fad (modifier)
  ()
  (:default-initargs
   :format-control "\\fad(~A,~A)"
   :arg1 1200
   :arg2 250))

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
   :arg1 "m 0 0 l 100 0 100 100 0 100"))

(defclass clip-drawing-scaled (modifier)
  ()
  (:default-initargs
   :format-control "\\clip(~A,~A)"
   :arg1 1
   :arg2 "m 0 0 l 100 0 100 100 0 100"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function modifiers extended
;;;
(defclass iclip-rectangle (modifier)
  ()
  (:default-initargs
   :format-control "\\iclip(~A,~A,~A,~A)"
   :arg1 1
   :arg2 1
   :arg3 100
   :arg4 100))

(defclass iclip-drawing (modifier)
  ()
  (:default-initargs
   :format-control "\\iclip(~A)"
   :arg1 "m 0 0 l 100 0 100 100 0 100"))

(defclass iclip-drawing-scaled (modifier)
  ()
  (:default-initargs
   :format-control "\\iclip(~A,~A)"
   :arg1 1
   :arg2 "m 0 0 l 100 0 100 100 0 100"))

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


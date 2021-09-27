(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtitle
;;;
(defmethod initialize-instance :after ((object subtitle) &rest initargs)
  (let ((sections (claraoke:lines object))
        (args (list* :allow-other-keys t initargs)))
    ;; Remove initarg for lines
    (remf args :lines)
    ;; Remove less specific keys
    (remf args :name)
    (remf args :margin-l)
    (remf args :margin-r)
    (remf args :margin-v)
    (setf (aref sections 0) (apply 'make-instance 'script-info args)
          (aref sections 1) (apply 'make-instance 'styles args)
          (aref sections 2) (apply 'make-instance 'events args)
          (aref sections 3) (apply 'make-instance 'fonts args)
          (aref sections 4) (apply 'make-instance 'graphics args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script info
;;;
(defmethod initialize-instance :after
    ((object script-info)
     &key
       title
       original-script
       original-translation
       original-editing
       original-timing
       sync-point
       scrypt-update-by
       update-details
       script-type
       collisions
       play-res-x
       play-res-y
       play-depth
       timer
       wrap-style
       scaled-border-and-shadow
       last-style-storage
       video-aspect-ratio
       video-zoom
       video-position
     &allow-other-keys)
  (let ((lines ()))
    (flet ((pushline (pretty-name value)
             (unless (null value)
               (pushnew (make-instance 'info :descriptor pretty-name :value value) lines
                        :key 'claraoke:descriptor
                        :test 'string-equal))))
      (pushnew (claraoke:note (claraoke-internal:script-note)) lines)
      (pushline "Title" title)
      (pushline "Original Script" original-script)
      (pushline "Original Translation" original-translation)
      (pushline "Original Editing" original-editing)
      (pushline "Original Timing" original-timing)
      (pushline "Sync Point" sync-point)
      (pushline "Scrypt Update By" scrypt-update-by)
      (pushline "Update Details" update-details)
      (pushline "ScriptType" script-type)
      (pushline "Collisions" collisions)
      (pushline "PlayResX" play-res-x)
      (pushline "PlayResY" play-res-y)
      (pushline "PlayDepth" play-depth)
      (pushline "Timer" timer)
      (pushline "WrapStyle" wrap-style)
      (pushline "ScaledBorderAndShadow" scaled-border-and-shadow)
      (pushline "Last Style Storage" last-style-storage)
      (pushline "Video Aspect Ratio" video-aspect-ratio)
      (pushline "Video Zoom" video-zoom)
      (pushline "Video Position" video-position)
      (setf (claraoke:lines object) lines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Styles
;;;
(defmethod initialize-instance :after ((object styles) &rest initargs &key style-name &allow-other-keys)
  (unless (null style-name)
    (remf initargs :descriptor)
    (pushnew (apply 'claraoke:style style-name initargs) (claraoke:lines object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Events
;;;
(defmethod initialize-instance :after ((object events) &rest initargs &key text &allow-other-keys)
  (unless (null text)
    (remf initargs :descriptor)
    (pushnew (apply 'claraoke:dialogue text initargs) (claraoke:lines object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fonts
;;;
(defmethod initialize-instance :after ((object fonts) &key font-filename &allow-other-keys)
  (unless (null font-filename)
    (pushnew (claraoke:info "fontname" :value font-filename)
             (claraoke:lines object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphics
;;;
(defmethod initialize-instance :after ((object graphics) &key graphic-filename &allow-other-keys)
  (unless (null graphic-filename)
    (pushnew (claraoke:info "filename" :value graphic-filename)
             (claraoke:lines object))))


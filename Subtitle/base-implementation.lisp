(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script (ASS)
;;;
(defmethod claraoke:print-script ((object subtitle) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (princ "[Script Info]" stream)
    (terpri stream)
    (princ "; Script generated by " stream)
    (princ (claraoke-internal:version) stream)
    (terpri stream)
    (claraoke:print-script (claraoke:.script-info object) stream)
    (terpri stream)
    (princ "[V4+ Styles]" stream)
    (terpri stream)
    (princ "Format: Name, " stream)
    (princ "Fontname, Fontsize, " stream)
    (princ "PrimaryColour, SecondaryColour, OutlineColour, BackColour, " stream)
    (princ "Bold, Italic, Underline, StrikeOut, " stream)
    (princ "ScaleX, ScaleY, " stream)
    (princ "Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, " stream)
    (princ "MarginL, MarginR, MarginV, " stream)
    (princ "Encoding" stream)
    (terpri stream)
    (claraoke:print-script (claraoke:styles object) stream)
    (terpri stream)
    (princ "[Events]" stream)
    (terpri stream)
    (princ "Format: Layer, " stream)
    (princ "Start, End, Style, Name, " stream)
    (princ "MarginL, MarginR, MarginV, " stream)
    (princ "Effect, Text" stream)
    (terpri stream)
    (claraoke:print-script (reverse (claraoke:events object)) stream)
    (terpri stream)
    object))

(defmethod claraoke:print-script ((object script-info) &optional stream)
  (let ((title (claraoke:title object))
        (original-script (claraoke:original-script object))
        (original-translation (claraoke:original-translation object))
        (original-editing (claraoke:original-editing object))
        (original-timing (claraoke:original-timing object))
        (sync-point (claraoke:sync-point object))
        (scrypt-update-by (claraoke:scrypt-update-by object))
        (update-details (claraoke:update-details object))
        (script-type (claraoke:script-type object))
        (collisions (claraoke:collisions object))
        (play-res-x (claraoke:play-res-x object))
        (play-res-y (claraoke:play-res-y object))
        (play-depth (claraoke:play-depth object))
        (timer (claraoke:timer object))
        (wrap-style (claraoke:wrap-style object))
        (scaled-border-and-shadow (claraoke:scaled-border-and-shadow object))
        (last-style-storage (claraoke:last-style-storage object))
        (video-aspect-ratio (claraoke:video-aspect-ratio object))
        (video-zoom (claraoke:video-zoom object))
        (video-position (claraoke:video-position object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (unless (null title)
      (princ "Title: " stream)
      (princ title stream)
      (terpri stream))
    (unless (null original-script)
      (princ "Original Script: " stream)
      (princ original-script stream)
      (terpri stream))
    (unless (null original-translation)
      (princ "Original Translation: " stream)
      (princ original-translation stream)
      (terpri stream))
    (unless (null original-editing)
      (princ "Original Editing: " stream)
      (princ original-editing stream)
      (terpri stream))
    (unless (null original-timing)
      (princ "Original Timing: " stream)
      (princ original-timing stream)
      (terpri stream))
    (unless (null sync-point)
      (princ "Sync Point: " stream)
      (princ sync-point stream)
      (terpri stream))
    (unless (null scrypt-update-by)
      (princ "Scrypt Update By: " stream)
      (princ scrypt-update-by stream)
      (terpri stream))
    (unless (null update-details)
      (princ "Update Details: " stream)
      (princ update-details stream)
      (terpri stream))
    (unless (null script-type)
      (princ "ScriptType: " stream)
      (princ script-type stream)
      (terpri stream))
    (unless (null collisions)
      (princ "Collisions: " stream)
      (princ collisions stream)
      (terpri stream))
    (unless (null play-res-x)
      (princ "PlayResX: " stream)
      (princ play-res-x stream)
      (terpri stream))
    (unless (null play-res-y)
      (princ "PlayResY: " stream)
      (princ play-res-y stream)
      (terpri stream))
    (unless (null play-depth)
      (princ "PlayDepth: " stream)
      (princ play-depth stream)
      (terpri stream))
    (unless (null timer)
      (princ "Timer: " stream)
      (princ timer stream)
      (terpri stream))
    (unless (null wrap-style)
      (princ "WrapStyle: " stream)
      (princ wrap-style stream)
      (terpri stream))
    (unless (null scaled-border-and-shadow)
      (princ "ScaledBorderAndShadow: " stream)
      (princ scaled-border-and-shadow stream)
      (terpri stream))
    (unless (null last-style-storage)
      (princ "Last Style Storage: " stream)
      (princ last-style-storage stream)
      (terpri stream))
    (unless (null video-aspect-ratio)
      (princ "Video Aspect Ratio: " stream)
      (princ video-aspect-ratio stream)
      (terpri stream))
    (unless (null video-zoom)
      (princ "Video Zoom: " stream)
      (princ video-zoom stream)
      (terpri stream))
    (unless (null video-position)
      (princ "Video Position: " stream)
      (princ video-position stream)
      (terpri stream))
    object))

(defmethod claraoke:print-script ((object list) &optional stream)
  (dolist (item object)
    (claraoke:print-script item stream))
  object)

(defmethod claraoke:print-script ((object style) &optional stream)
  (let ((name (claraoke:name object))
        (fontname (claraoke:fontname object))
        (fontsize (claraoke:fontsize object))
        (primary-colour (claraoke:primary-colour object))
        (secondary-colour (claraoke:secondary-colour object))
        (outline-colour (claraoke:outline-colour object))
        (back-colour (claraoke:back-colour object))
        (bold (claraoke:bold object))
        (italic (claraoke:italic object))
        (underline (claraoke:underline object))
        (strike-out (claraoke:strike-out object))
        (scale-x (claraoke:scale-x object))
        (scale-y (claraoke:scale-y object))
        (spacing (claraoke:spacing object))
        (angle (claraoke:angle object))
        (border-style (claraoke:border-style object))
        (outline (claraoke:outline object))
        (.shadow (claraoke:.shadow object))
        (alignment (claraoke:alignment object))
        (margin-l (claraoke:margin-l object))
        (margin-r (claraoke:margin-r object))
        (margin-v (claraoke:margin-v object))
        (encoding (claraoke:encoding object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (princ "Style: " stream)
    (princ name stream)
    (princ #\, stream)
    (princ fontname stream)
    (princ #\, stream)
    (princ fontsize stream)
    (princ #\, stream)
    (princ primary-colour stream)
    (princ #\, stream)
    (princ secondary-colour stream)
    (princ #\, stream)
    (princ outline-colour stream)
    (princ #\, stream)
    (princ back-colour stream)
    (princ #\, stream)
    (princ bold stream)
    (princ #\, stream)
    (princ italic stream)
    (princ #\, stream)
    (princ underline stream)
    (princ #\, stream)
    (princ strike-out stream)
    (princ #\, stream)
    (princ scale-x stream)
    (princ #\, stream)
    (princ scale-y stream)
    (princ #\, stream)
    (princ spacing stream)
    (princ #\, stream)
    (princ angle stream)
    (princ #\, stream)
    (princ border-style stream)
    (princ #\, stream)
    (princ outline stream)
    (princ #\, stream)
    (princ .shadow stream)
    (princ #\, stream)
    (princ alignment stream)
    (princ #\, stream)
    (princ margin-l stream)
    (princ #\, stream)
    (princ margin-r stream)
    (princ #\, stream)
    (princ margin-v stream)
    (princ #\, stream)
    (princ encoding stream)
    (terpri stream)
    object))

(defmethod claraoke:print-script ((object event) &optional stream)
  (let ((layer (claraoke:layer object))
        (start (claraoke:start object))
        (end (claraoke:end object))
        (.style (claraoke:.style object))
        (name (claraoke:name object))
        (margin-l (claraoke:margin-l object))
        (margin-r (claraoke:margin-r object))
        (margin-v (claraoke:margin-v object))
        (effect (claraoke:effect object))
        (text (claraoke:.text object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (princ (string-capitalize (type-of object)) stream)
    (princ ": " stream)
    (princ layer stream)
    (princ #\, stream)
    (princ start stream)
    (princ #\, stream)
    (princ end stream)
    (princ #\, stream)
    (princ .style stream)
    (princ #\, stream)
    (princ name stream)
    (princ #\, stream)
    (princ margin-l stream)
    (princ #\, stream)
    (princ margin-r stream)
    (princ #\, stream)
    (princ margin-v stream)
    (princ #\, stream)
    (princ effect stream)
    (princ #\, stream)
    (claraoke:print-script text stream)
    (terpri stream)
    object))


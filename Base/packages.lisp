(cl:in-package #:cl-user)

(defpackage #:claraoke
  (:use)
  (:export
   ;; claraoke-base
   #:print-script
   #:null-object-warning
   #:style-not-found
   #:failed-to-create-text
   #:failed-to-create-color
   #:failed-to-create-duration
   #:failed-to-create-subtitle
   #:failed-to-create-script-info
   #:failed-to-create-style
   #:failed-to-create-integer
   #:object-must-be-text
   #:object-must-be-color
   #:object-must-be-duration
   #:object-must-be-subtitle
   #:object-must-be-style
   #:object-must-be-event
   #:object-must-be-override
   #:object-must-be-integer
   ;; claraoke-text
   #:text
   #:index
   #:override
   #:overrides
   #:insert-override
   #:delete-override
   #:find-override
   #:sort-overrides
   ;; claraoke-color
   #:color
   #:colorstring
   #:random-color
   #:rgb
   #:red
   #:green
   #:blue
   #:alpha
   ;; claraoke-duration
   #:hours
   #:minutes
   #:seconds
   #:centiseconds
   #:duration
   #:durationp
   #:durationstring
   #:durationstringp
   #:durationinteger
   #:durationintegerp
   #:sync-duration
   #:increase-duration
   #:decrease-duration
   #:duration-lessp
   #:duration-greaterp
   #:duration-difference
   ;; claraoke-subtitle
   #:subtitle
   #:script-info
   #:.script-info
   #:script-type
   #:styles
   #:events
   #:dialogue
   #:comment
   #:picture
   #:sound
   #:movie
   #:command
   #:style
   #:insert-style
   #:delete-style
   #:find-style
   #:insert-event
   #:delete-event
   #:find-event
   #:last-event
   #:sort-events
   #:name
   #:title
   #:original-script
   #:original-translation
   #:original-editing
   #:original-timing
   #:sync-point
   #:scrypt-update-by
   #:update-details
   #:script-type
   #:collisions
   #:play-res-x
   #:play-res-y
   #:play-depth
   #:timer
   #:wrap-style
   #:scaled-border-and-shadow
   #:last-style-storage
   #:video-aspect-ratio
   #:video-zoom
   #:video-position
   #:fontname
   #:fontsize
   #:primary-colour
   #:secondary-colour
   #:outline-colour
   #:back-colour
   #:bold
   #:italic
   #:underline
   #:strike-out
   #:scale-x
   #:scale-y
   #:spacing
   #:angle
   #:border-style
   #:outline
   #:.shadow
   #:alignment
   #:margin-l
   #:margin-r
   #:margin-v
   #:encoding
   #:layer
   #:start
   #:end
   #:.style
   #:margin-l
   #:margin-r
   #:margin-v
   #:effect
   #:.text
   #:duration-length
   #| ... |#))

(defpackage #:claraoke-internal
  (:use #:common-lisp)
  (:export
   #:version
   #:mimic-accessor
   #:integer-from-string
   #:output-stream-from-designator
   #| ... |#))

(defpackage #:claraoke-base
  (:use #:common-lisp))


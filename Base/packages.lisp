(cl:in-package #:cl-user)

(defpackage #:claraoke
  (:use)
  (:export
   ;; claraoke-base
   #:version
   #:print-script
   #:style-not-found
   #:failed-to-create-duration
   #:failed-to-create-subtitle
   #:failed-to-create-style
   #:object-must-be-subtitle
   #:object-must-be-duration
   #:object-must-be-style
   #:object-must-be-event
   #:object-must-be-integer
   #:object-must-be-text
   #:object-must-be-override
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
   #:script-type
   #:styles
   #:events
   #:dialogue
   #:comment
   #:picture
   #:sound
   #:movie
   #:command
   #:insert-style
   #:delete-style
   #:find-style
   #:insert-event
   #:delete-event
   #:find-event
   #:last-event
   #:sort-event
   #:name
   #:title
   #:wrap-style
   #:play-res-x
   #:play-res-y
   #:scaled-border-and-shadow
   #:last-style-storage
   #:video-aspect-ratio
   #:video-zoom
   #:video-position
   #:original-translation
   #:collisions
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
   #:style
   #:.style
   #:margin-l
   #:margin-r
   #:margin-v
   #:effect
   #:text
   ;; claraoke-color
   #:color
   #:colorstring
   #:random-color
   #:rgb
   #:red
   #:green
   #:blue
   #:alpha
   ;; claraoke-text
   #:overrides
   #:override
   #:insert-override
   #:delete-override
   #:find-override
   #:position
   #:sort-overrides
   #:print-text
   #| ... |#))

(defpackage #:claraoke-internal
  (:use #:common-lisp)
  (:export
   #:output-stream-from-designator
   #| ... |#))

(defpackage #:claraoke-base
  (:use #:common-lisp))


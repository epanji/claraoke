(cl:in-package #:cl-user)

(defpackage #:claraoke
  (:use)
  (:export
   ;; claraoke-base
   #:version
   #:claraoke-error
   #:object-must-be-subtitle
   #:object-must-be-style
   #:object-must-be-event
   #:object-must-be-integer
   #:style-not-found
   ;; #:file-not-found
   ;; #:failed-writing-to-file
   ;; #:abort-writing-to-file
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
   #:increase-duration
   #:decrease-duration
   #:duration-lessp
   #:duration-greaterp
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
   #:print-subtitle
   #:insert-style
   #:delete-style
   #:find-style
   #:insert-event
   #:delete-event
   #:find-event
   #:last-event
   #:sort-event
   #:contents
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
   ;; #:name
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
   ;; #:name
   #:margin-l
   #:margin-r
   #:margin-v
   #:effect
   #:text
   #|..Mores..|#))

(defpackage #:claraoke-internal
  (:use #:common-lisp)
  (:export
   #|..Mores..|#))

(defpackage #:claraoke-base
  (:use #:common-lisp)
  (:export))


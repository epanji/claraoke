(cl:in-package #:cl-user)

(defpackage #:claraoke-subtitle
  (:use #:common-lisp)
  (:export
   #:*active-section*
   #:*generate-overrides-predicate*
   #:*ignore-note-predicate*
   #:*section-index*
   #:*spell-duration*
   #:*subtitle*
   #:command
   #:command-line-p
   #:comment
   #:comment-line-p
   #:create-object-from-string
   #:dialogue
   #:dialogue-line-p
   #:empty-line-p
   #:event
   #:events
   #:fonts
   #:graphics
   #:header-line-p
   #:info-line-p
   #:movie
   #:movie-line-p
   #:note-line-p
   #:picture
   #:picture-line-p
   #:script-info
   #:section-index
   #:section-line-p
   #:sort-event-predicate
   #:sound
   #:sound-line-p
   #:split-line-values
   #:style
   #:style-line-p
   #:styles
   #:subtitle
   #:unreadable-char-p))


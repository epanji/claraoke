(cl:in-package #:cl-user)

(defpackage #:claraoke-subtitle
  (:use #:common-lisp)
  (:intern
   #:*change-karaoke-type*
   #:*keep-original-modifier-predicate*
   #:*remove-unknown-modifier-predicate*
   #:*spell-duration*)
  (:export
   #:*active-section*
   #:*generate-overrides-predicate*
   #:*ignore-note-predicate*
   #:*section-index*
   #:*subtitle*
   #:command
   #:command-from-string
   #:comment
   #:comment-from-string
   #:create-object-from-string
   #:dialogue
   #:dialogue-from-string
   #:event
   #:events
   #:fonts
   #:graphics
   #:info
   #:info-from-string
   #:movie
   #:movie-from-string
   #:note
   #:note-from-string
   #:picture
   #:picture-from-string
   #:script-info
   #:section
   #:section-index
   #:sort-event-predicate
   #:sound
   #:sound-from-string
   #:split-line-values
   #:style
   #:style-from-string
   #:styles
   #:subtitle
   #:unreadable-char-p))


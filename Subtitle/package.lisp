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
   #:comment
   #:create-object-from-string
   #:dialogue
   #:event
   #:events
   #:fonts
   #:graphics
   #:info
   #:movie
   #:note
   #:picture
   #:script-info
   #:section
   #:section-index
   #:sort-event-predicate
   #:sound
   #:split-line-values
   #:style
   #:styles
   #:subtitle
   #:unreadable-char-p))


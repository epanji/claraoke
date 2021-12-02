(cl:in-package #:asdf-user)

(defsystem "claraoke"
  :version (:read-file-form "version.lisp-expr")
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "System to write KARAOKE typesetting according to ASS format. (Advanced Substation Alpha)"
  :license  ""
  :depends-on ("claraoke-base"
               "claraoke-text"
               "claraoke-color"
               "claraoke-duration"
               "claraoke-subtitle"))


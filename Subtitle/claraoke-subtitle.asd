(cl:in-package #:asdf-user)

(defsystem "claraoke-subtitle"
  :depends-on ("claraoke-base"
               "claraoke-text"
               "claraoke-color"
               "claraoke-duration")
  :serial t
  :components
  ((:file "package")
   (:file "classes")
   (:file "initializations")
   (:file "script-implementation")
   (:file "printer-implementation")
   (:file "text-implementation")
   (:file "color-implementation")
   (:file "duration-implementation")
   (:file "subtitle-implementation")
   (:file "parser-implementation")
   (:file "counter-implementation")))


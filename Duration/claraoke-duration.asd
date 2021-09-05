(cl:in-package #:asdf-user)

(defsystem "claraoke-duration"
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "package")
   (:file "duration-class")
   (:file "duration-implementation")
   (:file "printer-implementation")))


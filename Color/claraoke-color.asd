(cl:in-package #:asdf-user)

(defsystem #:claraoke-color
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "package")
   (:file "color-class")
   (:file "color-implementation")
   (:file "printer-implementation")))


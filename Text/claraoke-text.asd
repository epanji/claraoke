(cl:in-package #:asdf-user)

(defsystem "claraoke-text"
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "package")
   (:file "classes")
   (:file "text-parser")
   (:file "text-implementation")
   (:file "printer-implementation")))


(cl:in-package #:asdf-user)

(defsystem #:claraoke-user
  :depends-on ("claraoke")
  :serial t
  :components
  ((:file "package")
   (:file "claraoke-user")))


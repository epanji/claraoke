(cl:in-package #:asdf-user)

(defsystem #:claraoke-user
  :depends-on ("claraoke")
  :components ((:file "claraoke-user")))


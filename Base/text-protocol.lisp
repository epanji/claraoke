(cl:in-package #:claraoke-base)

(defgeneric claraoke:text (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:override (object index &rest initargs &key &allow-other-keys))

(defgeneric claraoke:insert-override (object override))

(defgeneric claraoke:delete-override (object override))

(defgeneric claraoke:find-override (object override))

(defgeneric claraoke:sort-overrides (text))


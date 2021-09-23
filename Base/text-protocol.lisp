(cl:in-package #:claraoke-base)

(defgeneric claraoke:text (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:override (object index &rest initargs &key &allow-other-keys))

(defgeneric claraoke:insert-override (object override))

(defgeneric claraoke:delete-override (object override))

(defgeneric claraoke:find-override (object override))

(defgeneric claraoke:increase-override (object &optional delta))

(defgeneric claraoke:decrease-override (object &optional delta))

(defgeneric claraoke:sort-overrides (text))

(defgeneric claraoke:increase-karaoke (object &optional delta))

(defgeneric claraoke:decrease-karaoke (object &optional delta))


(cl:in-package #:claraoke-base)

(defgeneric claraoke:text (object))

(defgeneric claraoke:override (position override-text))

(defgeneric claraoke:insert-override (object override))

(defgeneric claraoke:delete-override (object override))

(defgeneric claraoke:find-override (object override))

(defgeneric claraoke:sort-overrides (claraoke-text))


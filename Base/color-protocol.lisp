(cl:in-package #:claraoke-base)

(defgeneric claraoke:color (color)
  (:documentation "Return COLOR object from various COLOR argument."))

(defgeneric claraoke:colorp (color)
  (:documentation "Return BOOLEAN for COLOR object from COLOR argument."))

(defgeneric claraoke:colorstring (color)
  (:documentation "Return STRING represent acceptable color from COLOR argument."))

(defgeneric claraoke:colorstringp (color)
  (:documentation "Return BOOLEAN for STRING represent acceptable color from COLOR argument."))

(defgeneric claraoke:colorinteger (color)
  (:documentation "Return INTEGER with range 0 to 4294967295 from COLOR argument."))

(defgeneric claraoke:colorintegerp (color)
  (:documentation "Return BOOLEAN for INTEGER within range 0 to 4294967295 from COLOR argument."))

(defgeneric claraoke:alpha (object)
  (:documentation "Return INTEGER with range 0 to 255 from various OBJECT argument."))

(defgeneric claraoke:alphap (object)
  (:documentation "Return BOOLEAN for INTEGER with range 0 to 255 from OBJECT argument."))

(defgeneric claraoke:alphastring (object)
  (:documentation "Return STRING represent acceptable alpha from OBJECT argument."))

(defgeneric claraoke:alphastringp (object)
  (:documentation "Return BOOLEAN for STRING represent acceptable alpha from OBJECT argument."))

(defgeneric claraoke:rgb (red green blue &optional alpha)
  (:documentation "Return COLOR object from RED, GREEN, BLUE and optional ALPHA arguments."))

(defgeneric claraoke:hsv (hue saturation value)
  (:documentation "Return COLOR object from HUE, SATURATION, and VALUE arguments."))

(defgeneric claraoke:hsl (hue saturation lightness)
  (:documentation "Return COLOR object from HUE, SATURATION, and LIGHTNESS arguments."))

(defgeneric claraoke:hsvsl-list (color)
  (:documentation "Return list of HUE, SATURATION-VALUE, VALUE, SATURATION-LIGHTNESS, and LIGHTNESS from COLOR argument."))

(defgeneric claraoke:random-color (&optional alpha)
  (:documentation "Return COLOR object from random RGB and optional ALPHA argument."))

(defgeneric claraoke:synch-color (color source)
  (:documentation "Return COLOR object with synchronized values from various SOURCE argument.
The COLOR should be EQ with COLOR argument."))

(defgeneric claraoke:increase-color (color source)
  (:documentation "Return COLOR object with addition from various SOURCE argument.
The COLOR should be EQ with COLOR argument if the argument is COLOR object, otherwise the COLOR is new object.
If the value beyond upper or lower limit, ABS and MOD functions will modify the value."))

(defgeneric claraoke:decrease-color (color source)
  (:documentation "Return COLOR object with subtraction from various SOURCE argument.
The COLOR should be EQ with COLOR argument if the argument is COLOR object, otherwise the COLOR is new object.
If the value beyond upper or lower limit, ABS and MOD functions will modify the value."))

(defgeneric claraoke:bitwise-color (bitwise color1 color2)
  (:documentation "Return COLOR object from calling function from BITWISE argument with two integers from COLOR1 and COLOR2 arguments.
BITWISE could be a function which accept two arguments or member of keywords (:AND :ANDC1 :ANDC2 :EQV :IOR :NAND :NOR :ORC1 :ORC2 :XOR)."))

(defgeneric claraoke:combine-colors (alpha &rest colors)
  (:documentation "Return COLOR object after reduce colors with initial value black.
ALPHA argument will applied to all colors if it an acceptable alpha value.
Each alpha from colors will applied if ALPHA argument is NULL or keyword :EACH-ALPHA."))


(cl:in-package #:claraoke-test)

(in-suite color-suite)

(test color-from-integer
  (is (colorp (rgb 12 34 56)))
  (is (colorp (rgb 12 34 56 77))))

(test color-from-string-name
  (is (colorp (color "Wrong name lead to zero color")))
  (is (colorp (color "blue-violet")))
  (is (colorp (color "blue violet")))
  (is (colorp (color "Blue Violet")))
  (is (colorp (color "blueviolet")))
  (is (colorp (color "BlueViolet"))))

(test color-from-string-html
  (is (colorp (color "#ABC")))
  (is (colorp (color "#ABCD")))
  (is (colorp (color "#8A2BE2")))
  (is (colorp (color "#8A2BE24D"))))

(test color-from-string-spec
  (is (colorp (color "&HCBA&")))
  (is (colorp (color "&HDCBA")))
  (is (colorp (color "&HE22B8A&")))
  (is (colorp (color "&H4DE22B8A"))))

(test color-from-random
  (is (colorp (random-color)))
  (is (colorp (random-color 3/10))))

(test alpha-from-various-data
  (let ((obj (color "#8A2BE24D")))
    (is (integerp (alpha obj)))
    (is (integerp (alpha 77)))
    (is (integerp (alpha 3/10)))
    (is (integerp (alpha "4D")))))

(test string-alpha-from-various-data
  (is (stringp (alphastring 77)))
  (is (stringp (alphastring 3/4)))
  (is (stringp (alphastring "4D")))
  (is (stringp (alphastring "#4D")))
  (is (stringp (alphastring "&H4D")))
  (is (stringp (alphastring "&H4D&"))))

(test string-color-from-various-data
  (let ((obj (color "#8A2BE24D")))
    (is (stringp (colorstring obj)))
    (is (stringp (colorstring "Wrong name lead to zero color")))
    (is (stringp (colorstring "blue-violet")))
    (is (stringp (colorstring "blue violet")))
    (is (stringp (colorstring "Blue Violet")))
    (is (stringp (colorstring "blueviolet")))
    (is (stringp (colorstring "BlueViolet")))
    (is (stringp (colorstring "#8A2BE2")))
    (is (stringp (colorstring "#8A2BE24D")))
    (is (stringp (colorstring "&HE22B8A&")))
    (is (stringp (colorstring "&H4DE22B8A")))))

(test twin-digits-color-code
  (is (string= (colorstring "#ABC") (colorstring "#AABBCC")))
  (is (string= (colorstring "#ABCD") (colorstring "#AABBCCDD")))
  (is (string= (colorstring "&HCBA&") (colorstring "&HCCBBAA&")))
  (is (string= (colorstring "&HDCBA") (colorstring "&HDDCCBBAA")))
  (is (string= (alphastring "&HA&") (alphastring "&HAA&")))
  (is (string= (alphastring "&HA") (alphastring "&HAA"))))

(test expecting-color-error
  (signals error (colorstring 1234567))
  (signals error (colorstring #x8A2BE2)))


(cl:in-package #:claraoke-test)

(in-suite color-suite)

(test color-from-integer
  (is (colorp (rgb 12 34 56)))
  (is (colorp (rgb 12 34 56 77)))
  (is (colorp (color 305419895)))
  (is (colorp (color -305419895)))
  (is (colorp (color #x4D38220C))))

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

(test increase-decrease-color
  (let ((mutable1 (color "green"))
        (mutable2 (color "yellow")))
    (increase-color mutable1 "red")
    (is (= (colorinteger mutable1) (colorinteger mutable2))))
  (let ((mutable1 (color "green"))
        (mutable2 (color "yellow")))
    (decrease-color mutable2 "red")
    (is (= (colorinteger mutable1) (colorinteger mutable2))))
  (is (= (colorinteger (increase-color "green" "red"))
         (colorinteger "yellow")))
  (is (= (colorinteger (decrease-color "yellow" "red"))
         (colorinteger "green"))))

(test bitwise-color
  (is (= (colorinteger "&H1100BBAA")
         (colorinteger (bitwise-color :and "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&H22220000")
         (colorinteger (bitwise-color :andc1 "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HCCCC0000")
         (colorinteger (bitwise-color :andc2 "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HEEEE0001")
         (colorinteger (bitwise-color :eqv "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HFFEEBBAA")
         (colorinteger (bitwise-color :ior "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&H1100BBAB")
         (colorinteger (bitwise-color :nand "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HFFEEBBAB")
         (colorinteger (bitwise-color :nor "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HCCCC0001")
         (colorinteger (bitwise-color :orc1 "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&H22220001")
         (colorinteger (bitwise-color :orc2 "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "&HEEEE0000")
         (colorinteger (bitwise-color :xor "&HDDCCBBAA" "&H3322BBAA"))))
  (is (= (colorinteger "yellow")
         (colorinteger (bitwise-color #'+ "green" "red"))))
  (is (= (colorinteger "green")
         (colorinteger (bitwise-color #'- "yellow" "red")))))

(test combine-colors
  (is (string= (colorstring "black")
               (colorstring (combine-colors 255 "red")))) ; full red transparancy
  (is (string= (colorstring "red")
               (colorstring (combine-colors 0 "red"))))   ; full red opacity
  (is (string= (colorstring "#CC0000")
               (colorstring (combine-colors 51 "red"))))
  (is (string= (colorstring "#000033")
               (colorstring (combine-colors 204 "blue"))))
  (is (string= (colorstring "#CC0033")
               (colorstring (combine-colors 8/10 "red" "blue")))) ; bottom layer RED, top layer BLUE
  (is (string= (colorstring "#3300CC")
               (colorstring (combine-colors 8/10 "blue" "red"))))
  (is (string= (colorstring "#3300CC")
               (colorstring (combine-colors :each-alpha (rgb 0 0 255) (rgb 255 0 0 204)))))
  (is (string= (colorstring "#330029")
               (colorstring (combine-colors :each-alpha (rgb 0 0 255 204) (rgb 255 0 0 204)))))
  (is (string= (colorstring "#A32933")
               (colorstring (combine-colors 8/10 "red" "green" "blue"))))) ; reduce combine colors ((red green) blue)

(test hsvl-from-color
  (flet ((2fp (in)
           (float
            (/ (round (* 100 in))
               100))))
    (is (equal (mapcar #'2fp (hsvsl-list "#000000")) '(0.0 0.0 0.0 0.0 0.0)))
    (is (equal (mapcar #'2fp (hsvsl-list "#FFFFFF")) '(0.0 0.0 1.0 0.0 1.0)))
    (is (equal (mapcar #'2fp (hsvsl-list "#FF0000")) '(0.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#00FF00")) '(120.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#0000FF")) '(240.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#FFFF00")) '(60.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#00FFFF")) '(180.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#FF00FF")) '(300.0 1.0 1.0 1.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#BFBFBF")) '(0.0 0.0 0.75 0.0 0.75)))
    (is (equal (mapcar #'2fp (hsvsl-list "#808080")) '(0.0 0.0 0.5 0.0 0.5)))
    (is (equal (mapcar #'2fp (hsvsl-list "#800000")) '(0.0 1.0 0.5 1.0 0.25)))
    (is (equal (mapcar #'2fp (hsvsl-list "#808000")) '(60.0 1.0 0.5 1.0 0.25)))
    (is (equal (mapcar #'2fp (hsvsl-list "#008000")) '(120.0 1.0 0.5 1.0 0.25)))
    (is (equal (mapcar #'2fp (hsvsl-list "#800080")) '(300.0 1.0 0.5 1.0 0.25)))
    (is (equal (mapcar #'2fp (hsvsl-list "#008080")) '(180.0 1.0 0.5 1.0 0.25)))
    (is (equal (mapcar #'2fp (hsvsl-list "#000080")) '(240.0 1.0 0.5 1.0 0.25)))))

(test color-from-hsv
  (is (string= (colorstring "#000000") (colorstring (hsv 0 0 0))))
  (is (string= (colorstring "#FFFFFF") (colorstring (hsv 0 0 1))))
  (is (string= (colorstring "#FF0000") (colorstring (hsv 0 1 1))))
  (is (string= (colorstring "#00FF00") (colorstring (hsv 120 1 1))))
  (is (string= (colorstring "#0000FF") (colorstring (hsv 240 1 1))))
  (is (string= (colorstring "#FFFF00") (colorstring (hsv 60 1 1))))
  (is (string= (colorstring "#00FFFF") (colorstring (hsv 180 1 1))))
  (is (string= (colorstring "#FF00FF") (colorstring (hsv 300 1 1))))
  (is (string= (colorstring "#BFBFBF") (colorstring (hsv 0 0 0.75))))
  (is (string= (colorstring "#808080") (colorstring (hsv 0 0 0.5))))
  (is (string= (colorstring "#800000") (colorstring (hsv 0 1 0.5))))
  (is (string= (colorstring "#808000") (colorstring (hsv 60 1 0.5))))
  (is (string= (colorstring "#008000") (colorstring (hsv 120 1 0.5))))
  (is (string= (colorstring "#800080") (colorstring (hsv 300 1 0.5))))
  (is (string= (colorstring "#008080") (colorstring (hsv 180 1 0.5))))
  (is (string= (colorstring "#000080") (colorstring (hsv 240 1 0.5)))))

(test color-from-hsl
  (is (string= (colorstring "#000000") (colorstring (hsl 0 0 0))))
  (is (string= (colorstring "#FFFFFF") (colorstring (hsl 0 0 1))))
  (is (string= (colorstring "#FF0000") (colorstring (hsl 0 1 0.5))))
  (is (string= (colorstring "#00FF00") (colorstring (hsl 120 1 0.5))))
  (is (string= (colorstring "#0000FF") (colorstring (hsl 240 1 0.5))))
  (is (string= (colorstring "#FFFF00") (colorstring (hsl 60 1 0.5))))
  (is (string= (colorstring "#00FFFF") (colorstring (hsl 180 1 0.5))))
  (is (string= (colorstring "#FF00FF") (colorstring (hsl 300 1 0.5))))
  (is (string= (colorstring "#BFBFBF") (colorstring (hsl 0 0 0.75))))
  (is (string= (colorstring "#808080") (colorstring (hsl 0 0 0.5))))
  (is (string= (colorstring "#800000") (colorstring (hsl 0 1 0.25))))
  (is (string= (colorstring "#808000") (colorstring (hsl 60 1 0.25))))
  (is (string= (colorstring "#008000") (colorstring (hsl 120 1 0.25))))
  (is (string= (colorstring "#800080") (colorstring (hsl 300 1 0.25))))
  (is (string= (colorstring "#008080") (colorstring (hsl 180 1 0.25))))
  (is (string= (colorstring "#000080") (colorstring (hsl 240 1 0.25)))))

(test cmyk-from-color
  (flet ((2fp (in)
           (float
            (/ (round (* 100 in))
               100))))
    (is (equal (mapcar #'2fp (cmyk-list "black")) '(0.0 0.0 0.0 1.0)))
    (is (equal (mapcar #'2fp (cmyk-list "white")) '(0.0 0.0 0.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "red")) '(0.0 1.0 1.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "green")) '(1.0 0.0 1.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "blue")) '(1.0 1.0 0.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "yellow")) '(0.0 0.0 1.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "cyan")) '(1.0 0.0 0.0 0.0)))
    (is (equal (mapcar #'2fp (cmyk-list "magenta")) '(0.0 1.0 0.0 0.0)))))

(test color-from-cmyk
  (is (string= (colorstring "black") (colorstring (cmyk 0 0 0 1))))
  (is (string= (colorstring "white") (colorstring (cmyk 0 0 0 0))))
  (is (string= (colorstring "red") (colorstring (cmyk 0 1 1 0))))
  (is (string= (colorstring "green") (colorstring (cmyk 1 0 1 0))))
  (is (string= (colorstring "blue") (colorstring (cmyk 1 1 0 0))))
  (is (string= (colorstring "yellow") (colorstring (cmyk 0 0 1 0))))
  (is (string= (colorstring "cyan") (colorstring (cmyk 1 0 0 0))))
  (is (string= (colorstring "magenta") (colorstring (cmyk 0 1 0 0)))))

(test expecting-color-error
  (signals error (colorstring 1234567.89))
  (signals error (colorstring #c(123 456))))


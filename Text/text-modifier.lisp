(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style modifiers
;;;
(defun newline-modifier-p (input)
  (and (stringp input)
       (= 1 (length input))
       (or (char= #\n (elt input 0))
           (char= #\N (elt input 0)))))

(defun bold-modifier-p (input)
  (and (stringp input)
       (char= #\b (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun italic-modifier-p (input)
  (and (stringp input)
       (char= #\i (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun underline-modifier-p (input)
  (and (stringp input)
       (char= #\u (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun strikeout-modifier-p (input)
  (and (stringp input)
       (char= #\s (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun border-modifier-p (input)
  (and (stringp input)
       (< 4 (length input))
       (string= "bord" (subseq input 0 4))
       (char/= #\- (elt input 4))
       (claraoke-internal:number-string-p (subseq input 4))))

(defun shadow-modifier-p (input)
  (and (stringp input)
       (< 4 (length input))
       (string= "shad" (subseq input 0 4))
       (char/= #\- (elt input 4))
       (claraoke-internal:number-string-p (subseq input 4))))

(defun blur-edges-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "be" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun fontname-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "fn" (subseq input 0 2))))

(defun fontsize-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "fs" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun fontscale-x-modifier-p (input)
  (and (stringp input)
       (< 4 (length input))
       (string= "fscx" (subseq input 0 4))
       (claraoke-internal:number-string-p (subseq input 4))))

(defun fontscale-y-modifier-p (input)
  (and (stringp input)
       (< 4 (length input))
       (string= "fscy" (subseq input 0 4))
       (claraoke-internal:number-string-p (subseq input 4))))

(defun fontspace-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "fsp" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun fontrotate-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "fr" (subseq input 0 2))
       (claraoke-internal:number-string-p (subseq input 2))))

(defun fontrotate-x-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "frx" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun fontrotate-y-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "fry" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun fontrotate-z-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "frz" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun fontencoding-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "fe" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun color-modifier-p (input)
  (and (stringp input)
       (char= #\c (elt input 0))
       (string= "c" (subseq input 0 1))
       (or (> 2 (length input))
           (string-equal "&H" (subseq input 1 3)))))

(defun color1-modifier-p (input)
  (and (stringp input)
       (char= #\1 (elt input 0))
       (char= #\c (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun color2-modifier-p (input)
  (and (stringp input)
       (char= #\2 (elt input 0))
       (char= #\c (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun color3-modifier-p (input)
  (and (stringp input)
       (char= #\3 (elt input 0))
       (char= #\c (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun color4-modifier-p (input)
  (and (stringp input)
       (char= #\4 (elt input 0))
       (char= #\c (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun alpha-modifier-p (input)
  (and (stringp input)
       (char= #\a (elt input 0))
       (< 4 (length input))
       (string= "alpha" (subseq input 0 5))
       (or (> 6 (length input))
           (string-equal "&H" (subseq input 5 7)))))

(defun alpha1-modifier-p (input)
  (and (stringp input)
       (char= #\1 (elt input 0))
       (char= #\a (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun alpha2-modifier-p (input)
  (and (stringp input)
       (char= #\2 (elt input 0))
       (char= #\a (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun alpha3-modifier-p (input)
  (and (stringp input)
       (char= #\3 (elt input 0))
       (char= #\a (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun alpha4-modifier-p (input)
  (and (stringp input)
       (char= #\4 (elt input 0))
       (char= #\a (elt input 1))
       (or (> 3 (length input))
           (string-equal "&H" (subseq input 2 4)))))

(defun alignment-modifier-p (input)
  (and (stringp input)
       (char= #\a (elt input 0))
       (string= "a" (subseq input 0 1))
       (every 'digit-char-p (subseq input 1))))

(defun alignment-numpad-modifier-p (input)
  (and (stringp input)
       (< 2 (length input))
       (string= "an" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun karaoke-modifier-p (input)
  (and (stringp input)
       (char= #\k (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun karaoke-capital-modifier-p (input)
  (and (stringp input)
       (char= #\K (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun karaoke-fill-modifier-p (input)
  (and (stringp input)
       (char= #\k (elt input 0))
       (< 2 (length input))
       (string= "kf" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun karaoke-outline-modifier-p (input)
  (and (stringp input)
       (char= #\k (elt input 0))
       (< 2 (length input))
       (string= "ko" (subseq input 0 2))
       (every 'digit-char-p (subseq input 2))))

(defun wrapping-style-modifier-p (input)
  (and (stringp input)
       (char= #\q (elt input 0))
       (string= "q" (subseq input 0 1))
       (every 'digit-char-p (subseq input 1))))

(defun reset-modifier-p (input)
  (and (stringp input)
       (char= #\r (elt input 0))
       (or (= 1 (length input))
           (alpha-char-p (elt input 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style modifiers extended
;;;
(defun blur-modifier-p (input)
  (and (stringp input)
       (< 4 (length input))
       (string= "blur" (subseq input 0 4))
       (char/= #\- (elt input 4))
       (claraoke-internal:number-string-p (subseq input 4))))

(defun fontshear-x-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "fax" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun fontshear-y-modifier-p (input)
  (and (stringp input)
       (< 3 (length input))
       (string= "fay" (subseq input 0 3))
       (claraoke-internal:number-string-p (subseq input 3))))

(defun border-x-modifier-p (input)
  (and (stringp input)
       (< 5 (length input))
       (string= "xbord" (subseq input 0 5))
       (char/= #\- (elt input 5))
       (claraoke-internal:number-string-p (subseq input 5))))

(defun border-y-modifier-p (input)
  (and (stringp input)
       (< 5 (length input))
       (string= "ybord" (subseq input 0 5))
       (char/= #\- (elt input 5))
       (claraoke-internal:number-string-p (subseq input 5))))

(defun shadow-x-modifier-p (input)
  (and (stringp input)
       (< 5 (length input))
       (string= "xshad" (subseq input 0 5))
       (claraoke-internal:number-string-p (subseq input 5))))

(defun shadow-y-modifier-p (input)
  (and (stringp input)
       (< 5 (length input))
       (string= "yshad" (subseq input 0 5))
       (claraoke-internal:number-string-p (subseq input 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function modifiers
;;;
(defun transformation1-modifier-p (input &aux pos)
  (and (stringp input)
       (find #\( input)
       (char= #\t (elt input 0))
       (char= #\( (elt input 1))
       (setf pos (position #\\ input))
       (= 0 (count #\, (subseq input 0 pos)))))

(defun transformation2-modifier-p (input &aux pos)
  (and (stringp input)
       (find #\( input)
       (char= #\t (elt input 0))
       (char= #\( (elt input 1))
       (setf pos (position #\\ input))
       (= 1 (count #\, (subseq input 0 pos)))))

(defun transformation3-modifier-p (input &aux pos)
  (and (stringp input)
       (find #\( input)
       (char= #\t (elt input 0))
       (char= #\( (elt input 1))
       (setf pos (position #\\ input))
       (= 2 (count #\, (subseq input 0 pos)))))

(defun transformation4-modifier-p (input &aux pos)
  (and (stringp input)
       (find #\( input)
       (char= #\t (elt input 0))
       (char= #\( (elt input 1))
       (setf pos (position #\\ input))
       (= 3 (count #\, (subseq input 0 pos)))))

(defun move-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 6 (length input))
       (char= #\( (elt input 4))
       (<= 3 (count #\, input))
       (string= "move" (subseq input 0 4))))

(defun pos-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 5 (length input))
       (char= #\( (elt input 3))
       (= 1 (count #\, input))
       (string= "pos" (subseq input 0 3))))

(defun origin-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 5 (length input))
       (char= #\( (elt input 3))
       (= 1 (count #\, input))
       (string= "org" (subseq input 0 3))))

(defun fade-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 6 (length input))
       (char= #\( (elt input 4))
       (= 6 (count #\, input))
       (string= "fade" (subseq input 0 4))))

(defun fad-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 5 (length input))
       (char= #\( (elt input 3))
       (= 1 (count #\, input))
       (string= "fad" (subseq input 0 3))))

(defun clip-rectangle-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 6 (length input))
       (char= #\( (elt input 4))
       (= 3 (count #\, input))
       (string= "clip" (subseq input 0 4))))

(defun clip-drawing-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 6 (length input))
       (char= #\( (elt input 4))
       (= 0 (count #\, input))
       (some 'digit-char-p input)
       (string= "clip" (subseq input 0 4))))

(defun clip-drawing-scaled-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 6 (length input))
       (char= #\( (elt input 4))
       (= 1 (count #\, input))
       (string= "clip" (subseq input 0 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function modifiers extended
;;;
(defun iclip-rectangle-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 7 (length input))
       (char= #\( (elt input 5))
       (= 3 (count #\, input))
       (string= "iclip" (subseq input 0 5))))

(defun iclip-drawing-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 7 (length input))
       (char= #\( (elt input 5))
       (= 0 (count #\, input))
       (some 'digit-char-p input)
       (string= "iclip" (subseq input 0 5))))

(defun iclip-drawing-scaled-modifier-p (input)
  (and (stringp input)
       (find #\( input)
       (< 7 (length input))
       (char= #\( (elt input 5))
       (= 1 (count #\, input))
       (string= "iclip" (subseq input 0 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing modifiers
;;;
(defun drawing-mode-modifier-p (input)
  (and (stringp input)
       (char= #\p (elt input 0))
       (every 'digit-char-p (subseq input 1))))

(defun drawing-baseline-offset-modifier-p (input)
  (and (stringp input)
       (char= #\p (elt input 0))
       (< 3 (length input))
       (string= "pbo" (subseq input 0 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifier from string
;;;
(defun modifier-from-string (input)
  (setf input (string-trim "\\" input))
  (flet ((subseqi (in s &optional e)
           (claraoke-internal:number-or-string (subseq in s e)))
         (keyargs (in &rest keywords)
           (let ((string (subseq in (1+ (or (position #\( in) 0)) (1- (length in)))))
             (apply 'claraoke-internal:split-by-char-to-keys #\, string keywords))))
    (cond ((newline-modifier-p input)
           (make-instance 'newline :index nil :arg1 (char= #\N (elt input 0))))
          ((bold-modifier-p input)
           (make-instance 'bold :arg1 (subseqi input 1)))
          ((italic-modifier-p input)
           (make-instance 'italic :arg1 (subseqi input 1)))
          ((underline-modifier-p input)
           (make-instance 'underline :arg1 (subseqi input 1)))
          ((strikeout-modifier-p input)
           (make-instance 'strikeout :arg1 (subseqi input 1)))
          ((border-modifier-p input)
           (make-instance 'border :arg1 (subseqi input 4)))
          ((shadow-modifier-p input)
           (make-instance '.shadow :arg1 (subseqi input 4)))
          ((blur-edges-modifier-p input)
           (make-instance 'blur-edges :arg1 (subseqi input 2)))
          ((fontname-modifier-p input)
           (make-instance 'fontname :arg1 (subseqi input 2)))
          ((fontsize-modifier-p input)
           (make-instance 'fontsize :arg1 (subseqi input 2)))
          ((fontscale-x-modifier-p input)
           (make-instance 'fontscale-x :arg1 (subseqi input 4)))
          ((fontscale-y-modifier-p input)
           (make-instance 'fontscale-y :arg1 (subseqi input 4)))
          ((fontspace-modifier-p input)
           (make-instance 'fontspace :arg1 (subseqi input 3)))
          ((fontrotate-modifier-p input)
           (if *keep-original-modifier-predicate*
               (make-instance 'fontrotate :arg1 (subseqi input 2))
               (make-instance 'fontrotate-z :arg1 (subseqi input 2))))
          ((fontrotate-x-modifier-p input)
           (make-instance 'fontrotate-x :arg1 (subseqi input 3)))
          ((fontrotate-y-modifier-p input)
           (make-instance 'fontrotate-y :arg1 (subseqi input 3)))
          ((fontrotate-z-modifier-p input)
           (make-instance 'fontrotate-z :arg1 (subseqi input 3)))
          ((fontencoding-modifier-p input)
           (make-instance 'fontencoding :arg1 (subseqi input 2)))
          ((color-modifier-p input)
           (if *keep-original-modifier-predicate*
               (make-instance 'color :arg1 (subseqi input 1))
               (make-instance 'color1 :arg1 (subseqi input 1))))
          ((color1-modifier-p input)
           (make-instance 'color1 :arg1 (subseqi input 2)))
          ((color2-modifier-p input)
           (make-instance 'color2 :arg1 (subseqi input 2)))
          ((color3-modifier-p input)
           (make-instance 'color3 :arg1 (subseqi input 2)))
          ((color4-modifier-p input)
           (make-instance 'color4 :arg1 (subseqi input 2)))
          ((alpha-modifier-p input)
           (if *keep-original-modifier-predicate*
               (make-instance 'alpha :arg1 (subseqi input 5))
               (make-instance 'alpha1 :arg1 (subseqi input 5))))
          ((alpha1-modifier-p input)
           (make-instance 'alpha1 :arg1 (subseqi input 2)))
          ((alpha2-modifier-p input)
           (make-instance 'alpha2 :arg1 (subseqi input 2)))
          ((alpha3-modifier-p input)
           (make-instance 'alpha3 :arg1 (subseqi input 2)))
          ((alpha4-modifier-p input)
           (make-instance 'alpha4 :arg1 (subseqi input 2)))
          ((alignment-modifier-p input)
           (make-instance 'alignment :arg1 (subseqi input 1)))
          ((alignment-numpad-modifier-p input)
           (make-instance 'alignment-numpad :arg1 (subseqi input 2)))
          ((karaoke-modifier-p input)
           (make-instance 'karaoke :arg1 (subseqi input 1)))
          ((karaoke-capital-modifier-p input)
           (if *keep-original-modifier-predicate*
               (make-instance 'karaoke-capital :arg1 (subseqi input 1))
               (make-instance 'karaoke-fill :arg1 (subseqi input 1))))
          ((karaoke-fill-modifier-p input)
           (make-instance 'karaoke-fill :arg1 (subseqi input 2)))
          ((karaoke-outline-modifier-p input)
           (make-instance 'karaoke-outline :arg1 (subseqi input 2)))
          ((wrapping-style-modifier-p input)
           (make-instance 'wrapping-style :arg1 (subseqi input 1)))
          ((reset-modifier-p input)
           (make-instance 'reset :arg1 (subseqi input 1)))
          ;; Style extended
          ((blur-modifier-p input)
           (make-instance 'blur :arg1 (subseqi input 4)))
          ((fontshear-x-modifier-p input)
           (make-instance 'fontshear-x :arg1 (subseqi input 3)))
          ((fontshear-y-modifier-p input)
           (make-instance 'fontshear-y :arg1 (subseqi input 3)))
          ((border-x-modifier-p input)
           (make-instance 'border-x :arg1 (subseqi input 5)))
          ((border-y-modifier-p input)
           (make-instance 'border-y :arg1 (subseqi input 5)))
          ((shadow-x-modifier-p input)
           (make-instance 'shadow-x :arg1 (subseqi input 5)))
          ((shadow-y-modifier-p input)
           (make-instance 'shadow-y :arg1 (subseqi input 5)))
          ;; Function
          ((transformation1-modifier-p input)
           (apply 'make-instance 'transformation1 (keyargs input :arg1)))
          ((transformation2-modifier-p input)
           (apply 'make-instance 'transformation2 (keyargs input :arg1 :arg2)))
          ((transformation3-modifier-p input)
           (apply 'make-instance 'transformation3 (keyargs input :arg1 :arg2 :arg3)))
          ((transformation4-modifier-p input)
           (apply 'make-instance 'transformation4 (keyargs input :arg1 :arg2 :arg3 :arg4)))
          ((move-modifier-p input)
           (apply 'make-instance 'move (keyargs input :arg1 :arg2 :arg3 :arg4 :arg5 :arg6)))
          ((pos-modifier-p input)
           (apply 'make-instance 'pos (keyargs input :arg1 :arg2)))
          ((origin-modifier-p input)
           (apply 'make-instance 'origin (keyargs input :arg1 :arg2)))
          ((fade-modifier-p input)
           (apply 'make-instance 'fade (keyargs input :arg1 :arg2 :arg3 :arg4 :arg5 :arg6 :arg7)))
          ((fad-modifier-p input)
           (apply 'make-instance 'fad (keyargs input :arg1 :arg2)))
          ((clip-rectangle-modifier-p input)
           (apply 'make-instance 'clip-rectangle (keyargs input :arg1 :arg2 :arg3 :arg4)))
          ((clip-drawing-modifier-p input)
           (apply 'make-instance 'clip-drawing (keyargs input :arg1)))
          ((clip-drawing-scaled-modifier-p input)
           (apply 'make-instance 'clip-drawing-scaled (keyargs input :arg1 :arg2)))
          ;; Function extended
          ((iclip-rectangle-modifier-p input)
           (apply 'make-instance 'iclip-rectangle (keyargs input :arg1 :arg2 :arg3 :arg4)))
          ((iclip-drawing-modifier-p input)
           (apply 'make-instance 'iclip-drawing (keyargs input :arg1)))
          ((iclip-drawing-scaled-modifier-p input)
           (apply 'make-instance 'iclip-drawing-scaled (keyargs input :arg1 :arg2)))
          ;; Drawing
          ((drawing-mode-modifier-p input)
           (make-instance 'drawing-mode :arg1 (subseqi input 1)))
          ((drawing-baseline-offset-modifier-p input)
           (make-instance 'drawing-baseline-offset :arg1 (subseqi input 3)))
          ;; Default
          (t (unless *remove-unknown-modifier-predicate*
               (make-instance 'unknown :arg1 input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Batch overrides
;;;
(defun override-from-string (override-text &optional (index 0))
  (if (char= #\{ (elt override-text 0))
      (let* ((inputs (split-modifier override-text))
             (override (make-instance 'batch :index index))
             (modifiers (loop for input in (reverse inputs)
                              for modifier = (modifier-from-string input)
                              unless (null modifier)
                                collect modifier)))
        (setf (claraoke:modifiers override)
              (remove-duplicates
               modifiers
               :test 'duplicate-modifier-p
               :from-end t))
        override)
      (let ((override (modifier-from-string override-text)))
        (setf (claraoke:index override) index)
        override)))


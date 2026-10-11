;;; inspect/color-chart.lisp -- print a terminal colour chart.
;;;   sbcl --script inspect/color-chart.lisp
;;; Shows the 16 basic colours, the 256-colour palette, and 24-bit
;;; truecolour gradients, so you can check what your terminal renders.

(defvar *esc* (string (code-char 27)))

(defun sgr (params) (format nil "~A[~Am" *esc* params))
(defun sgr-reset () (sgr "0"))

(defun print-basic ()
  (format t "~&Basic 16 -- foreground (on default bg), then background~%")
  (format t "  fg normal:  ")
  (loop for c from 30 to 37 do (format t "~A ~2D " (sgr (format nil "~D" c)) c))
  (format t "~A~%" (sgr-reset))
  (format t "  fg bright:  ")
  (loop for c from 90 to 97 do (format t "~A ~2D " (sgr (format nil "~D" c)) c))
  (format t "~A~%" (sgr-reset))
  (format t "  bg normal:  ")
  (loop for c from 40 to 47 do (format t "~A ~2D " (sgr (format nil "~D" c)) c))
  (format t "~A~%" (sgr-reset))
  (format t "  bg bright:  ")
  (loop for c from 100 to 107 do (format t "~A ~3D " (sgr (format nil "~D" c)) c))
  (format t "~A~%~%" (sgr-reset)))

(defun print-256 ()
  (format t "~&256-colour palette (48;5;n)  0-15 system, 16-231 cube, 232-255 grey~%")
  (loop for n from 0 below 256
        do (format t "~A~3D" (sgr (format nil "48;5;~D" n)) n)
           (when (= (mod n 16) 15) (format t "~A~%" (sgr-reset))))
  (format t "~%"))

(defun print-truecolor ()
  (format t "~&24-bit truecolour gradients (48;2;r;g;b)~%")
  (dotimes (x 72)
    (let ((r (round (* 255 (/ x 71.0)))))
      (format t "~A " (sgr (format nil "48;2;~D;~D;~D" r (- 255 r) 128)))))
  (format t "~A~%" (sgr-reset))
  (dotimes (x 72)
    (let ((g (round (* 255 (/ x 71.0)))))
      (format t "~A " (sgr (format nil "48;2;~D;~D;~D" 0 g (- 255 g))))))
  (format t "~A~%" (sgr-reset)))

(print-basic)
(print-256)
(print-truecolor)

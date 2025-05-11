(in-package :metrical-hierarchies)

;; * Utilities copied from Michael Edwards' Slippery Chicken
;;; Since I only need a select few of Slippery Chicken's functions, I decided  
;;; to copy them rather than add SC as a dependency. Some of the following  
;;; functions were written by me. If no author is specified, assume it was
;;; written by Michael.
;;;
;;; Code copied on March 25th 2025

;; ** utilities

;; *** decider

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/decider
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;;
;;; DESCRIPTION
;;; Return an index, which can be used to select an element from a sequence,
;;; when provided with a list of weights (see the example). It does that by
;;; scaling the selector argument relative to the sum of all weights, using
;;; rescale. Then it goes through all the weights and as soon as the selector
;;; is smaller than the sum of the weights so far, the index of the current
;;; weight is returned. So when given a list of weights '(2 1), the following
;;; selectors will return:
;;; 0     => 0
;;; 1/3   => 0
;;; 19/30 => 0
;;; 2/3   => 1
;;; 1     => 1
;;; This process is thus deterministic. By providing a random number as a
;;; selector, you can make random choices etc.
;;; 
;;; ARGUMENTS
;;; - A number between 0 and 1
;;; - A list of numbers, representing weights 
;;; 
;;; RETURN VALUE
;;; index of chosen element
;;;
;;; EXAMPLE
#|
;;; simple example, choosing from a list:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (nth (decider 0.1 weights) ls))
=> d4

;;; making a simple melody, following a sine wave:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop for i from 0 to pi by 0.25 collect
       (nth (decider (abs (sin i)) weights) ls)))
=> (C4 E4 F4 G4 B4 B4 B4 B4 B4 A4 G4 F4 D4)

;;; make a random melody with 10 pitches:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop repeat 10 collect
       (nth (decider (random 1.0) weights) ls)))
|#
;;; SYNOPSIS
(defun decider (selector weights)
;;; ****  
  (labels ((helper (selector ls1 index sum)
             (cond ((null ls1) (1- (length weights)))
                   ((< selector sum) index)
                   (t (helper selector
                              (cdr ls1)
                              (+ index 1)
                              (+ sum (rationalize (car ls1))))))))
    (helper (sc::rescale (rationalize selector) 0 1 0 (loop for i in weights sum
                                                       (rationalize i)))
            (cdr weights) 0 (rationalize (car weights)))))

;; *** rescale

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rescale
;;; DATE
;;; June 8th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a value within an original range, return its value withing a new range
;;; 
;;; ARGUMENTS
;;; - the value we want to rescale
;;; - the original minimum
;;; - the original maximum
;;; - the new minimum
;;; - the new maximum
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :out-of-range. The function to call when the first argument is not within
;;; the range of arguments two and three. This would normally be #'error,
;;; #'warn or NIL. If #'warn or NIL, argument 1 will be hard-limited to the
;;; original range. Default = #'error
;;; :type-of-result. Usually this function uses float precision, but by setting
;;; type-of-result to #'double-float or #'rationalize, it is more precise.
;;; 
;;; RETURN VALUE
;;; The value within the new range (a number)
;;; 
;;; EXAMPLE
#|
(rescale .5 0 1 0 100)
==> 50.0
|#
;;; SYNOPSIS
(defun rescale (val min max new-min new-max &optional (out-of-range #'error)
                                              (type-of-result #'float))
;;; ****
  (flet ((oor () ; in case we need to call it on more than one occasion...
           (when (functionp out-of-range)
             (funcall out-of-range
                      "utilities::rescale: first argument (~a) should be ~
                       within the ~%original range (~a to ~a)" val min max))))
    (when (or (>= min max)
              (>= new-min new-max))
      (error "utilities::rescale: argument 2 (~a) must be < argument 3 (~a) ~
              ~%and sim. for argument 4 (~a) and 5 (~a)"
             min max new-min new-max))
    (unless (and (>= val min)
                 (<= val max))
      (oor)
      (setf val (if (> val max) max min)))
    (let* ((range1 (funcall type-of-result (- max min)))
           (range2 (funcall type-of-result (- new-max new-min)))
           (prop (funcall type-of-result (/ (- val min) range1))))
      (+ new-min (* prop range2)))))

;; *** flatten

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/flatten
;;; DESCRIPTION
;;; Return a list of nested lists of any depth as a flat list.
;;; 
;;; ARGUMENTS
;;; - A list of nested lists.
;;; 
;;; RETURN VALUE
;;; A flat list.
;;; 
;;; EXAMPLE
#|
(flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))

=> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)

|#
;;; SYNOPSIS
(defun flatten (nested-list)
;;; ****
  (cond ((null nested-list) nil)
        ((atom nested-list) (list nested-list))
        (t (append (flatten (first nested-list))
                   (flatten (rest nested-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* utilities/interpolate
;;; DESCRIPTION
;;; Get the interpolated value at a specified point within an envelope. The
;;; envelope must be specified in the form of a list of break-point pairs.
;;; 
;;; ARGUMENTS
;;; - A number that is the point within the specified envelope for which to
;;;   return the interpolated value.
;;; - A list of break-point pairs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :scaler. A number that is the factor by which to scale the values of
;;;   the break-point pairs in the given envelope before retrieving the
;;;   interpolated value. Default = 1.
;;; - :exp. A number that is the exponent to which the result should be
;;;   raised. Default = 1.
;;; - :warn. T or NIL to indicate whether the method should print a warning if
;;;   the specified point is outside of the bounds of the x-axis specified in
;;;   the list of break-point pairs. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using the defaults
(interpolate 50 '(0 0 100 1))

=> 0.5

;;; Specifying a different scaler
(interpolate 50 '(0 0 100 1) :scaler 2)

=> 1.0

;;; Specifying a different exponent by which the result is to be raised
(interpolate 50 '(0 0 100 1) :exp 2)

=> 0.25

|#
;;; SYNOPSIS
(defmethod interpolate (point (env list) &key (scaler 1) (exp 1) (warn t))
;;; ****
  "e.g. (interpolate 50 '(0 0 100 1) :scaler .5 :exp 2)
   => 0.0625
   The :EXP arg is the exponent that the interpolation result should
   be raised to."
  ;; MDE Thu Jul 14 21:29:59 2016 -- could happen...
  (if (not env)
    point
    (let ((lastx (lastx env))
          (lasty (first (last env))))
      (cond ((> point lastx)
             (when warn
               (warn "interpolate: ~a is off the x axis of ~%~a~
                      ~%returning last y value: ~a"
                     point env lasty))
             lasty)
            ((< point (car env))
             (let (y1)
               ;; (error "interpolate: Can't interpolate ~a in ~a" point env))
               ;; MDE Thu Apr 23 09:48:57 2020, Heidhausen -- if our x values
               ;; start > the point we're looking for, return the first y value
               (warn "utilities::interp-aux: envelope starts later than point!~
                      ~%Returning first y value: ~a" (setq y1 (second env)))
               y1))
            (t (interp-aux point env scaler exp))))))

;; *** visualize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/visualize
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;; 
;;; DESCRIPTION
;;; Print a Visualization of an array or a list into the repl with 64 values
;;; -> *ascii art* <-
;;; 
;;; ARGUMENTS
;;; - An array or a list
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :y-range. Maximum value that the y-axis display. When nil, the graph will be
;;; normalized.
;;; :start. Where to start reading the sequence from. Default = 0 
;;; :abs. When t, the absolute value of all numbers is visualized
;;; :scale. If the sequence is shorter than 64 and scale is t, the graph will
;;; be scaled to 64 values
;;; 
;;; RETURN VALUE
;;; ":)"
;;;
;;; EXAMPLE
#|
(visualize (loop repeat 64 for i from 0 by 0.1 collect (sin i)))
(visualize (loop repeat 128 for i from 0 by 0.1 collect (sin i)) :start 64)
(visualize (loop repeat 128 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :start 96)
(visualize (loop repeat 55 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :abs t :y-range 1)
|#
;;; SYNOPSIS
(defun visualize (ls &key y-range (start 0) abs (scale t))
;;; ****
  (when (arrayp ls)
    (setf ls (loop for i across ls collect i)))
  (when abs (setf ls (loop for i in ls collect (abs i))))
  (let* ((matrix (make-array '(64 17) :initial-element 0.0))
         (maxi (apply #'max (mapcar #'abs ls)))
         (y-range (if y-range y-range
                      (if (= maxi 0) 1 maxi)))
         (len (length ls))
         (size (if (or scale (>= (- len start) 64)) 64 (- len start))))
    (loop for i from start below (+ size start) do
         (loop for j below 17 do
              (if (= (round (+ (* (/ (nth (mod (floor
                                                (+ start
                                                   (if scale
                                                       (* (/ i size)
                                                          (- len start))
                                                       i)))
                                               len)
                                          ls)
                                     y-range)
                                  8 (if abs 2 1))
                               (* 8 (if abs 0 1))))
                     j)
                  (setf (aref matrix (- i start) j) 1)
                  (setf (aref matrix (- i start) j) 0))))
    (loop for j downfrom 16 to 0 do
         (print (apply 'concatenate 'string
                       (loop for i below 64 collect
                            (if (= (aref matrix i j)  1)
                                "_"
                                " ")))))
    "=)"))


;; ** rthm-seq-bar

;; *** rqq-num-divisions

(defun rqq-num-divisions (rqq)
  (loop for divs in rqq
	sum (typecase divs
	      (list (first divs))
	      (number divs) ; LF: changed to numberp
	      ;; LF: changed rthm-seq::rqq-denom: to rqq-num-divisions:
	      (t (error "rqq-num-divisions: malformed data: ~a in ~%~a"
			divs rqq)))))

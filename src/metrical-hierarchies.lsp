(in-package :metrical-hierarchies)

;; * Metrical Hierarchy

;; ** get-metrical-depth-simple
;;; Determine the metrical weight of a rational number, corresponding to Fred
;;; Lerdahl and Ray Jackendoff's idea of Metric Structure. In this simpler
;;; implementation, the distance between strong beats at each metrical level
;;; is always two. This is adequate for a 4/4 meter without tuplets.
;;; 
;;; Arguments:
;;; - p: A rational number representing a beat within a measure, where the
;;;   length of the measure is always 1. Assumed to be a fractional value
;;;   between 0 and 1; otherwise, (mod p 1) is applied.
;;; - how-many-levels (optional, default: 4): An integer between 2 and 99,
;;;   defining the number of hierarchical levels.
;;;
;;; Returns the hierarchical level (integer) for the input number. The input is
;;; quantized to a beat, and the output reflects the strength of that beat in a
;;; 4/4 time signature. The stronger the beat is felt, the higher the
;;; hierarchical level (i.e., the output is smaller).
;;;
;;; This function has a certain similarity to Thomae's function, as it returns
;;; the denominator of a rational number after simplifying. However, this is
;;; just a superficial comparison, as that function serves an entirely different
;;; purpose, and we also do some rounding and take the log2 of the output.
;;;
;;; (loop for i from 0 to 1 by .125 collect (get-metrical-depth-simple i))
;;; => (0 3 2 3 1 3 2 3 0)
;;;
;;; (visualize (loop for i from 0 to 1 by 1/64
;;;	         collect (get-metrical-depth-simple i 7)))
;;; " _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" 
;;; "  _   _   _   _   _   _   _   _   _   _   _   _   _   _   _   _ " 
;;; "                                                                " 
;;; "    _       _       _       _       _       _       _       _   " 
;;; "        _               _               _               _       " 
;;; "                _                               _               " 
;;; "                                                                " 
;;; "                                _                               " 
;;; "_                                                               "
;;;
(defun get-metrical-depth-simple (p &optional (how-many-levels 4))
  (check-type p number)
  (unless (and (integerp how-many-levels) (< 1 how-many-levels 100))
    (error "get-metrical-depth-simple: value for how-many-levels not an ~
            integer between 2 and 99: ~a" how-many-levels))
  (let* ((nr-of-possible-beats (expt 2 (1- how-many-levels)))
	 (beat-rational (rational (/ (round (* (mod p 1) nr-of-possible-beats))
				     nr-of-possible-beats))))
    (log (denominator beat-rational) 2)))

;; ** get-metrical-depth
;;; Determine the metrical weight of a rational number, corresponding to Fred
;;; Lerdahl and Ray Jackendoff's idea of Metric Structure. The distance between
;;; strong beats at each metrical level can be set.
;;; 
;;; Arguments:
;;; - p: A rational number representing a beat within a measure, where the
;;;   length of the measure is always 1. Assumed to be a fractional value
;;;   between 0 and 1; otherwise, (mod p 1) is applied.
;;; - list-of-divisions (optional, default: '(2 2 2 2)): A list of integers
;;;   between 2 and 50. The first number gives the distance between strong beats
;;;   at the highest metrical level. The second number gives the distance
;;;   between strong beats at the second-highest level, and so on. The length of
;;;   this list determines the number of hierarchical levels. The list should
;;;   have between 1 and 99 elements. For a 3/4 time signature, this could be
;;;   defined as: '(3 2 2).
;;;
;;; Returns the hierarchical level (integer) for the input number. The input is
;;; quantized to a beat, and the output reflects the strength of that beat. The
;;; stronger the beat is felt, given the metrical divisions, the higher the
;;; hierarchical level (i.e., the output is smaller).
;;;
;;; (loop for i from 0 to 1 by .125 collect (get-metrical-depth i))
;;; => (0 3 2 3 1 3 2 3 0)
;;;
;;; (loop for i from 0 to 1 by 1/12 collect (get-metrical-depth i '(3 2 2)))
;;; => (0 3 2 3 1 3 2 3 1 3 2 3 0)
;;;
;;; (visualize (loop for i from 0 to 1 by 1/60
;;; 		 collect (get-metrical-depth i '(3 5 2 2))))
;;; "  _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _ _ _ " 
;;; "                                                                " 
;;; "   _   _   _   _   _    _   _   _   _   _    _   _   _   _   _  " 
;;; "                                                                " 
;;; "     _   _   _   _        _   _   _   _        _   _   _   _    " 
;;; "                                                                " 
;;; "                     __                   __                    " 
;;; "                                                                " 
;;; "__                                                             _" 
;;; "                                                                " 
;;;
(defun get-metrical-depth (p &optional (list-of-divisions '(2 2 2 2)))
  (check-type p number)
  (check-type list-of-divisions list)
  (unless (and (<= 1 (length list-of-divisions) 99)
	       (every (lambda (n) (and (numberp n) (<= 2 n 50))) ; changed to numberp
		      list-of-divisions))
    (error "get-metrical-depth: malformed list-of-divisions: ~a"
	   list-of-divisions))
  (let* ((nr-of-possible-beats (apply #'* list-of-divisions))
	 (beat-rational (rational (/ (round (* (mod p 1) nr-of-possible-beats))
				     nr-of-possible-beats))))
    (loop for result from 0
	  and mul in list-of-divisions
	  when (= 0 (mod beat-rational 1)) return result
	    do (setf beat-rational (* beat-rational mul))
	  finally (return (1+ result)))))

;; ** get-metrical-depth-divisibility
;;; Determine the metrical weight of a rational number, by dividing a pulse
;;; length equally (without remainder). If the number of the beat in question
;;; is a multiple of any of those dividers, its metrical weight increases.
;;; 
;;; Arguments:
;;; - p: A rational number representing a beat within a measure, where the
;;;   length of the measure is always 1. Assumed to be a fractional value
;;;   between 0 and 1; otherwise, (mod p 1) is applied.
;;; - pulse-length (optional, default: 12): An integer. How many pulses/beats
;;;   are in one measure. Every pulse/beat then has a duration of
;;;   1/pulse-length. Chosing pulse-lengths that can be divided by many numbers
;;;   is best, like 12 or 24. 
;;;
;;; Returns the hierarchical level (integer) for the input number. The input is
;;; quantized to a beat, and the output reflects the strength of that beat. The
;;; stronger the beat is determined to be, the higher the hierarchical level
;;; (i.e., the output is smaller).
;;;
;;; (loop for i from 0 to 1 by 1/12 collect (get-metrical-depth-divisibility i))
;;; => (0 5 4 4 3 5 2 5 3 4 4 5 0)
;;;
;;; (visualize (loop for i from 0 to 1 by 1/48
;;; 	 collect (get-metrical-depth-divisibility i 48)))
;;;
;;; "  _    _  _    _ __    _ __    _ _    __ _    __ _    _  _    _ " 
;;; "   ___      ___    __       ___   ___       __    ___      ___  " 
;;; "      _                    _         _                    _     " 
;;; "        __ _            _               _            _ __       " 
;;; "                _    __                   __    _               " 
;;; "                                                                " 
;;; "                                _                               " 
;;; "                                                                " 
;;; "__                                                             _" 
;;; "                                                                " 
;;; "                                                                "
;;;
(defun get-metrical-depth-divisibility (p &optional (pulse-length 12))
  (check-type p number)
  (check-type pulse-length number)
  (unless (<= pulse-length 3628800)
    (error "get-metrical-depth-divisibility: pulse-length is too large: ~a"
           pulse-length))
  (let* ((beat-position (round (* (mod p 1) pulse-length)))
         (max-divisors 0)
         (matching-divisors 0))
    (loop for div from 1 below pulse-length
          when (zerop (mod pulse-length div))
            do (incf max-divisors)
               (when (zerop (mod beat-position (/ pulse-length div)))
                 (incf matching-divisors)))
    (- max-divisors matching-divisors)))

;; * Indispensibility
;; ** rqq-to-indispensability-list
;;; Get the indispensability values for each pulse/beat in a stratified meter,
;;; according to Clarence Barlow and Bernd Härpfer. However, here the
;;; indispensability values are inverted, so that the most important beat is
;;; always 0! The use of RQQ notation has no immediate benefit for this
;;; function. You could just as well use what Bernd Härpfer has called GNSM.
;;; #'gnsm-to-indispensability-list does just that. The benefits of
;;; RQQ become clear when looking at #'rqq-to-indispensability-function.
;;;
;;; Arguments:
;;; - rqq: A nested list of elements. The nesting structure of this list
;;;   represents the hierarchical tree of a stratified meter. Any rhythm in
;;;   RQQ notation is a valid input. However, since only the nesting structure
;;;   is of interest in this function, the elements within the RQQ list don't
;;;   need to reflect actual numbers; they don't even need to be numbers.
;;;
;;; Returns a list of numbers. Each number stands for a pulse/beat in a
;;; stratified meter. Each pulse is assigned a unique number representing its
;;; metric weight. For an in-depth description of what the function does,
;;; consult the in-line comments in each function.
;;;
;;; Examples:
#|
(rqq-to-indispensability-list '(4 (1 1 1 1)))
=> (0 3 2 1)

(rqq-to-indispensability-list '(4 ((1 (1)) 1 1 1)))
=> (0 3 2 1)

(rqq-to-indispensability-list '(5 ((1 (1 1)) 1 1 1)))
=> (0 4 3 2 1)

(rqq-to-indispensability-list '(5 (1 (1 (1 1)) 1 1)))
=> (0 3 4 2 1)

(rqq-to-indispensability-list '(8 ((2 (1 1)) (3 (1 1 1)) (2 (1 1)))))
=> (0 5 2 6 4 1 3)

(rqq-to-indispensability-list '(5 (1 1 1 1 1)))
=> (0 4 3 2 1)

(rqq-to-indispensability-list '(7 (1 (2 (1 1 1)) 1 (3 (1 1)))))
=> (0 3 6 5 2 1 4)

(rqq-to-indispensability-list '(7 ((3 (1 (2 (1 1 1)))) (4 (1 (3 (1 1)))))))
=> (0 3 6 5 1 2 4)

(rqq-to-indispensability-list '(7 ((6 ((2 (1 1)) 1 (3 (1 1 1)))) (1 (1)))))
=> (0 5 3 2 6 4 1)

(rqq-to-indispensability-list '(4 ((2 ((2 (1 1)) (2 (1 1)))) (2 ((2 (1 1)) (2 (1 1)))))))
=> (0 7 3 5 1 6 2 4)

(rqq-to-indispensability-list ')
=> (0 11 5 9 3 7 1 10 4 8 2 6)
|#
(defun rqq-to-indispensability-list (rqq)
  ;; Convert RQQ to GNSM, then apply gnsm-to-indispensability-list
  (gnsm-to-indispensability-list (rqq-to-gnsm rqq)))

;; *** gnsm-to-indispensability-list
;;; This is called by #'rqq-to-indispensability-list. See the Documentation for
;;; that function.
(defun gnsm-to-indispensability-list (gnsm)
  ;; sanity checks
  (check-type gnsm list)
  (unless (every #'numberp gnsm)
    (error "gnsm-to-indispensability-list: ~
            expected a list of numbers!"))
  ;; most variables
  (let* ((len (length gnsm))
	 (result (make-list len :initial-element -1))
	 (indices '())
	 (old-indices '())
	 (set-indices '())
	 (remaining-indices '())
	 (layer (apply #'max gnsm)))  ; which layer is being processed
    ;; some auxiliary functions to save space below,
    ;; most need the context of the variables above.
    (labels ((set-indices ()
	       (setf indices
		     (loop for i from 0 and e in gnsm
			   when (= e layer) collect i)))
	     ;; get the fundamental-indispensability values for any number
	     ;; of pulses-
	     (fundamental-indispensability (len)
	       (when (> len 0)
		 (cons (1- len) (loop for i from 0 below (1- len) collect i))))
	     ;; get the next index for a value was already set
	     (next-set-index (idx)
	       (setf idx (mod (1+ idx) len))
	       (if (>= (nth idx result) 0)
		   idx
		   (next-set-index idx)))
	     ;; copy values from higher strata to an empty slot to their left
	     ;; all indices that are assigned a value are pushed into set-values
	     (copy-from-neighbours (indices)
	       (setf set-indices '())
	       (loop for i from 0 below (length indices) do
		 (let* ((idx (nth i indices))
			(next (next-set-index idx)))
		   (when (or (= (1+ i) (length indices))
			     (< idx next (nth (1+ i) indices)))
		     (push idx set-indices)
		     (setf (nth idx result) (nth next result))))))
	     ;; return the set indices from least to most important
	     (sort-copied-indices ()
	       (let ((order '()))
		 (loop while (< (length order) (length set-indices))
		       for n from 0
		       do (loop for i in set-indices
				when (= n (nth i result)) do (push i order)))
		 (reverse order))))
      ;; get the indices for the top layer
      (set-indices)
      (setf old-indices indices)
      ;; Step 2) of Härfpers extended indispensability algorithm: 
      ;; Assigning the fundamental indispensability values for the top layer,
      ;; however all strata can contain any number of pulses.
      (loop for e in (fundamental-indispensability (length indices))
	    and i in indices
	    do (setf (nth i result) e))
      ;; Step 3) of the algorithm, iterate through all layers.
      (loop until (< layer 0) do
	;; Check, whether all indices for the current layer have been assigned
	(setf remaining-indices
	      (loop for i in indices
		    unless (>= (nth i result) 0) collect i))
	;; Step 3 a)
	(unless remaining-indices
	  (decf layer)
	  (set-indices)
	  (copy-from-neighbours indices))
	;; Step 3 c)
	(when remaining-indices
	  (copy-from-neighbours remaining-indices))
	;; Transform copied values, so that all values are unique.
	(loop for idx in (sort-copied-indices) with val = 0 do
	  (setf (nth idx result) val)
	  (incf val))
	;; Step 3 b) and d)
	(loop for i in old-indices do (incf (nth i result) (length set-indices)))
	;; All set-indices are now processed and considered old-indices
	(setf old-indices (append old-indices set-indices))))
    ;; flip result, so that the most important value is 0
    (let ((max (apply #'max result)))
      (loop for i in result collect (- max i)))))

;; alias
(setf (symbol-function 'mnsm-to-indispensability-list)
      #'gnsm-to-indispensability-list)

;; *** rqq-to-gnsm
;;; Extract the metrical hierarchy from RQQ notation.
;;;
;;; Arguments:
;;; - rqq: A nested list of elements. The nesting structure of this list
;;;   represents the hierarchical tree of a stratified meter. Any rhythm in
;;;   RQQ notation is a valid input. However, since only the nesting structure
;;;   is of interest in this function, the elements within the RQQ list don't
;;;   need to reflect actual numbers; they don't even need to be numbers.
;;;
;;; Returns a list of numbers. Each number stands for a pulse/pulse in a
;;; stratified meter. The higher the number, the higher the level that number
;;; belongs to.
;;; (Bernd Härpfer calls this GNSM: General Notation for Stratified Meters.
;;; technically this function returns the MNSM: Meassure Notation for Stratified
;;; Meters, as the first number is always the highest, thus indicating the
;;; length of the meassure over the entire list. #'rqq-to-mnsm is an alias to
;;; rqq-to-gnsm.)
;;;
;;; Examples:
#|
(rqq-to-gnsm '(4 (1 1 1 1)))
=> (1 0 0 0)

(rqq-to-gnsm '(4 ((1 (1)) 1 1 1)))
=> (1 0 0 0)

(rqq-to-gnsm '(5 ((1 (1 1)) 1 1 1)))
=> (2 0 1 1 1)

(rqq-to-gnsm '(7 ((3 (1 (2 (1 1 1)))) (4 (1 (3 (1 1)))))))
=> (3 1 0 0 2 1 0)

(rqq-to-gnsm '(7 (1 (2 (1 1 1)) 1 (3 (1 1)))))
=> (2 1 0 0 1 1 0)

(rqq-to-gnsm '(7 ((6 ((2 (1 1)) 1 (3 (1 1 1)))) (1 (1)))))
=> (3 0 1 1 0 0 2)
|#
(defun rqq-to-gnsm (rqq)
  ;; sanity checks
  (check-type rqq list)
  (unless (= 2 (length rqq))
    (error "rqq-to-indispensability-list: rqq list malformed! expected two ~
            elements, but got ~a" (length rqq)))
  ;; This helper function does all the work of receiving the metrichal hierarchy
  ;; Goes through all nested lists. The deeper the nesting is, the higher of a
  ;; number the elements get, but since the first element is also part of
  ;; a higher metrical level, decrease its number by 1.
  (labels ((helper (rqq lvl)
	     (let ((ls (loop for el in rqq
			     append (if (atom el)
					(list lvl)
					(helper (second el) (1+ lvl))))))
	       (decf (first ls))
	       ls)))
    ;; here we only invert the output of the helper function, so that the pulses
    ;; that are part of the highest levels are represented by higher numbers.
    (let* ((tmp (helper (second rqq) 1))
	   (max (apply #'max tmp)))
      (loop for e in tmp collect (- max e)))))

;; alias
(setf (symbol-function 'rqq-to-mnsm) #'rqq-to-gnsm)

;; ** get-metrical-indispensability
;;; Determine the metrical weight of a rational number, corresponding to
;;; Clarence Barlow's idea of Metric Structure, or what he called
;;; indispensability. In contrast to Fred Lerdahl and Ray Jackendoff's ideas,
;;; every beat is assigned a unique weight. Bernd Härpfer (and independently
;;; Marc Evanstein) extended Barlow's idea to also work with non-isochronous
;;; (additive) meters. RQQ allows us to even declare meters for which no
;;; metric level is isochronous, meaning even the lowest level (the pulse)
;;; does not need to be regular. Rather, the pulse is identical to the rhythm
;;; that the RQQ sequence describes. Look at the graphs below.
;;;
;;; Arguments:
;;; - p: A rational number representing a beat within a measure, where the
;;;   length of the measure is always 1. Assumed to be a fractional value
;;;   between 0 and 1; otherwise, (mod p 1) is applied.
;;; - rqq: Description of the metrical hierarchy using RQQ notation. The
;;;   metrical hierarchy is defined by the nesting structure of the RQQ list.
;;;   The proporion of the number relates to the proportion of the metric beats
;;;   / the pulse. Some simple examples: '(4 (1 1 1 1)) is four regular beats,
;;;   '(3 (1 1 1)) is three regular beats. However, '(4 (1 1 2)) is two beats
;;;   followed by one beat twice their duration.
;;; - interpolate (optional, default: NIL): When T, interpolate between weights.
;;;
;;; Returns a number representing the importance of that position in a measure.
;;; These values are inverted so that the most important beat is always 0.
;;;
;;; To avoid the overhead of always recalculating the indispensability function,
;;; consider using #'defun-indispensability. The function you define can then be
;;; used in the same way as #'get-metrical-indispensability.
;;; Examples:
;;;
;;; (visualize
;;;  (loop for i from 0 below 1 by 1/64
;;;        collect (get-metrical-indispensability i '(4 (1 1 1 1)))))
;;;
;;; "                ________________                                " 
;;; "                                                                " 
;;; "                                                                " 
;;; "                                ________________                " 
;;; "                                                                " 
;;; "                                                ________________" 
;;; "                                                                " 
;;; "                                                                " 
;;; "________________                                                "
;;;
;;; (visualize
;;;  (loop for i from 0 below 1 by 1/64
;;;        collect (get-metrical-indispensability i '(2 (1 1 3)))))
;;;
;;; "             _____________                                      " 
;;; "                                                                " 
;;; "                                                                " 
;;; "                                                                " 
;;; "                          ______________________________________" 
;;; "                                                                " 
;;; "                                                                " 
;;; "                                                                " 
;;; "_____________                                                   " 
;;; "                                                                "
;;;
;;; (visualize
;;;  (loop for i from 0 below 1 by 1/64
;;;        collect (get-metrical-indispensability
;;; 		i
;;; 		'(7 (1 (2 (1 1 1)) 1 (3 (1 1)))))))
;;;
;;; "                ______                                          " 
;;; "                      ______                                    " 
;;; "                                                                " 
;;; "                                                   _____________" 
;;; "          ______                                                " 
;;; "                            _________                           " 
;;; "                                                                " 
;;; "                                     ______________             " 
;;; "__________                                                      " 
;;; "                                                                " 
;;; "                                                                " 
;;; "                                                                " 
;;; "                                                                "
;;;
(defun get-metrical-indispensability (p rqq &optional interpolate)
  (funcall (rqq-to-indispensability-function rqq interpolate) p))

;; *** defun-indispensability
;;; Similar to get-metrical-indispensability, but less overhead.
;;; Use like this:
#|
(defun-indispensability my-function '(4 (1 1 (2 (1 2)))))

(loop for i from 0 below 1 by 1/12 collect (my-function i))
=> (0 0 0 2 2 2 1 1 3 3 3 3)
|#
(defmacro defun-indispensability (name rqq &optional interpolate)
  `(setf (fdefinition ',name)
	 (rqq-to-indispensability-function ,rqq ,interpolate)))

;; *** rqq-to-indispensability-function
;;; Returns an anonymous function that's to be used as described in the
;;; documentation for #'get-metrical-indispensability.
(defun rqq-to-indispensability-function (rqq &optional interpolate)
  (let* ((dur-list (rqq-to-durations rqq 1))
	 (sum  (loop for i in dur-list sum i))
	 (indisp-ls (rqq-to-indispensability-list rqq))
	 (env '()))
    ;; normalized dur-list, all durations sum to 1
    (setf dur-list (mapcar #'(lambda (x) (/ x sum)) dur-list))
    ;; collect into an envelope, makes interpolation easier.
    (when interpolate
      (setf env (loop for dur in (append dur-list '(0))
		      and y in (append indisp-ls (first (list indisp-ls)))
		      collect x collect y sum dur into x)))
    (if interpolate
	(lambda (x) (interpolate (mod x 1) env))
	(lambda (x) (nth (decider (mod x 1) dur-list) indisp-ls)))))

;; *** rqq-to-durations
;;; This is a stripped verion of Michael Edwards' #'rqq-divide-aux,
;;; which can be found in slippery-chicken/src/rthm-seq-bar.lsp (25.03.25)
;;; I don't need to handle rests and notation.
(defun rqq-to-durations (divisions parent-dur)
  (if (numberp divisions) ; changed to numberp
      ;; divisions is an integer; return 
      (/ divisions parent-dur)
      ;; divisions is not an integer:
      (let* ((2divs (second divisions))
             (rqqnd (rqq-num-divisions 2divs))
	     ;; MDE Sat Feb  8 13:04:05 2020 -- Dan and Jolon discovered that
	     ;; if we try this (make-rthm-seq-bar '((3 8) (1.5 (1 1 1 1 1))))
	     ;; then it doesn't work, but if that 1.5 is expressed as 3/2,
	     ;; then it does (floating-point precision problem perhaps?). So
	     ;; force the duration to be a rational
             (this-dur (rational (first divisions)))
	     ;; the ratio of the total number of divisions we have to the
	     ;; duration  
             (pd (/ (* parent-dur rqqnd) this-dur)))
        (flatten 
	 (loop for div in 2divs
	       collect (rqq-to-durations div pd))))))

;; EOF metrical-hierarchy.lsp

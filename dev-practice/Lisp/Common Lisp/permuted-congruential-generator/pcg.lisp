(defpackage #:pcg
  (:use #:cl)
  (:export
   #:rng-pcg-with
   #:pcg32-random
   #:with-n-bit-mask
   #:with-bit-mask
   )
  )

(in-package #:pcg)

(defparameter +default-state+ 0) ;; This can be any number
(defparameter +default-multiplier+ 6364136223846793005) ;; This must be congruent to 1 mod 4
(defparameter +default-increment+ 1442695040888963407)  ;; This must be an odd number

(defstruct rng-pcg
  state
  multiplier
  increment
  )

(defun rng-pcg-with (&key state multiplier increment)
  (make-rng-pcg :state (if state state (+ (if increment increment +default-increment+) +default-state+))
                :multiplier (if multiplier multiplier +default-multiplier+)
                :increment (if increment increment +default-increment+)
                )
  )

(defun n-bit-mask (n)
  (1- (expt 2 n))
  )

(defmacro with-n-bit-mask (n expr)
  "Applies an N bit mask to the result of the expression."
  `(logand (n-bit-mask ,n) ,expr)
  )

(defmacro with-bit-mask (bit-mask expr)
  "Applies a bit mask to the result of the expression."
  `(logand ,bit-mask ,expr)
  )

(defun pcg32-random (rng)
  (unless (rng-pcg-p rng)
    (error "Not a rng-pcg object.")
    )
  (let* ((oldstate (rng-pcg-state rng))
         (xorshifted (with-n-bit-mask 32 (ash (logxor (ash oldstate -18) oldstate) -27)))
         (rot (with-n-bit-mask 32 (ash oldstate -59)))
         (result
          (with-n-bit-mask 32
            (logior
             (ash xorshifted (- rot))
             (ash xorshifted (with-n-bit-mask 5
                               (- (ash 1 32) rot))))
            )
           )
         )
    (setf (rng-pcg-state rng) (with-n-bit-mask 64
                                (+ (* (rng-pcg-multiplier rng)
                                      oldstate)
                                   (logior 1 (rng-pcg-increment rng)))))
    result
    )
  )


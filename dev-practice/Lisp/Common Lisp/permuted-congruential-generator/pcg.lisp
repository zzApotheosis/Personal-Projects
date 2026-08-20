(defpackage #:pcg
  (:use #:cl)
  (:export
   #:rng-pcg-with
   #:pcg32-random
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

(defun pcg32-random (rng)
  (unless (rng-pcg-p rng)
    (error "Not a rng-pcg object.")
    )
  (let* ((oldstate (rng-pcg-state rng))
         (xorshifted (logand (ash (logxor (ash oldstate -18) oldstate) -27) (n-bit-mask 32)))
         (rot (logand (ash oldstate -59) (n-bit-mask 32)))
         (result
          (logand
           (logior
            (ash xorshifted (- rot))
            (ash xorshifted (logand
                             (- (ash 1 32) rot)
                             (n-bit-mask 5))))
           (n-bit-mask 32)
           )
          )
         )
    (setf (rng-pcg-state rng) (logand
                               (+ (* (rng-pcg-multiplier rng)
                                     oldstate)
                                  (logior 1 (rng-pcg-increment rng)))
                               (n-bit-mask 64)))
    result
    )
  )


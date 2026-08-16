(defun is-divisible (n limit)
  "Ensures that all integers from 1 to limit is a factor of n."
  (let ((result t))
    (loop for i from 1 to limit
          do
          (unless (= (mod n i) 0)
            (setf result nil)
            (return)
            )
          )
    result
    )
  )

(defun smallest-num-with-n-divisors (n)
  (loop with i = 1
        do
        (when (is-divisible i n)
          (return i)
          )
        (incf i)
        )
  )

(defun original-solution ()
  "A horribly optimized solution to Project Euler Problem 5. Seriously, please don't ever use this solution for anything."
  (let* ((n 20)
         (expected 232792560)
         (actual (smallest-num-with-n-divisors n)))
    (assert (= expected actual))
    actual
    )
  )

#|

The above code is what I wrote as a brute-force method to solve this problem.
I understand that it is incredibly slow and unoptimized.
Researching the problem beyond my initial approach, I found that this is a fundamental
mathematical problem that involves computing the Least Common Multiple of numbers in sequence
from 1 to n. The example gives 2520 as the answer to the smallest integer that can be divided
evenly by each number between 1 and 10.

[1] current-lcm = 1
[2] current-lcm = 2
[3] current-lcm = 6
[4] current-lcm = 12
[5] current-lcm = 60
[6] current-lcm = 60
[7] current-lcm = 420
[8] current-lcm = 840
[9] current-lcm = 2520
[10] current-lcm = 2520

|#

(defun solve (n)
  (let ((current-lcm 1))
    (loop for i from 1 to n
          do
          (setf current-lcm (lcm current-lcm i))
          )
    current-lcm
    )
  )

(defun solution ()
  (let* ((n 20)
         (expected 232792560)
         (actual (solve n))
         )
    (assert (= expected actual))
    actual
    )
  )


(declare-const x1 Real)
(declare-const x2 Real)

(assert (<= (+ (* x1 1) (* x2 2)) 14))
(assert (<= (+ (* x1 -3) (* x2 1)) 0))
(assert (= (- x1 x2) 2))

(maximize (+ (* 3 x1) (* 4 x2)))
(check-sat)
(get-objectives)

(get-model)

(declare-const x1 Real)
(declare-const x2 Real)

(declare-const z Real)

(declare-const y1 Real)
(declare-const y2 Real)
(declare-const y3 Real)


(assert (<= (+ (* x1 1) (* x2 2)) 14))
(assert (<= (+ (* x1 -3) (* x2 1)) 0))
(assert (<= (- x1 x2) 2))
(assert (= z (+ (* 3 x1) (* 4 x2))))

(assert (>= (+ (* y1 2) (* y2 1) (* y3 -1)) 4))
(assert (>= (+ (* y1 1) (* y2 -3) (* y3 1)) 3))
(assert (= z (+ (* y1 14) (* y3 2))))
 
(maximize z)

(check-sat)
(get-objectives)

(get-model)


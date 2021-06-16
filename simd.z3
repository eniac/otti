(declare-const y1 Real)
(declare-const y2 Real)
(declare-const y3 Real)

(declare-const dual Real)

(assert (>= (+ (* y1 2) (* y2 1) (* y3 -1)) 4))
(assert (>= (+ (* y1 1) (* y2 -3) (* y3 1)) 3))
(assert (= dual (+ (* y1 14) (* y3 2))))

(minimize dual)


(check-sat)
(get-objectives)

(get-model)

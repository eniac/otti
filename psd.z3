(declare-const x11 Real)
(declare-const x12 Real)
(declare-const x21 Real)
(declare-const x22 Real)
(declare-const r11 Real)
(declare-const r12 Real)
(declare-const r22 Real)

(assert (= x11 (+ (* r11 r11) (* r12 r12))))
(assert (= x12 (+ (* r11 0) (* r12 r22))))
(assert (= x21 (+ (* 0 r11) (* r22 r12))))
(assert (= x22 (+ (* 0 0) (* r22 r22))))

(assert (< 0 r11))
(assert (< 0 r22))



(check-sat)
(get-objectives)

(get-model)




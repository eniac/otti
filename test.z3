(declare-const x11 Real)
(declare-const x12 Real)
(declare-const x21 Real)
(declare-const x22 Real)

(declare-const z Real)
(declare-const gap Real)

(declare-const y1 Real)
(declare-const y2 Real)

(declare-const s11 Real)
(declare-const s12 Real)
(declare-const s21 Real)
(declare-const s22 Real)

(assert (= (+ (* 0.8255 x11) (* 0.0980 x12) (* 0.0980 x21) (* 0.9839 x22)) 0.4234))
(assert (= (+ (* -0.7169 x11) (* -0.1079 x12) (* -0.1079 x21) (* 0.4916 x22)) 0.0459))


(assert (< 0 x11))
(assert (< 0 (- (* x11 x22) (* x12 x21))))
(assert (= x12 x21))

(assert (< 0 s11))
(assert (< 0 (- (* s11 s22) (* s12 s21))))
(assert (= s12 s21))

(assert (= (+ (* y1 0.8255) (* y2  -0.7169) s11) 0.3007))
(assert (= (+ (* y1 0.0980) (* y2 -0.1079) s12) 0.6385))
(assert (= (+ (* y1 0.0980) (* y2 -0.1079) s21) 0.6385))
(assert (= (+ (* y1 0.9839) (* y2 0.4916) s22) -0.7562))

;(assert (= z (+ (* 0.3007 x11) (* 0.6385 x12) (* 0.6385 x21) (* -0.7562 x22))))
(assert (= gap (+ (* y1 0.4234) (* y2 0.0459))))
;(assert (< gap 2))
;(assert (> gap -2))

;(assert (= yb (+ (* y1 -2.3831) (* y2 0.8521))))

;(assert (= (- z yb) 0))

;(assert (= (+ (* s11 x11) (* s12 x12) (* s21 x21) (* s22 x22)) gap))
;(assert (<= gap 0.5))

;(assert (= (+ (* 0.3007 x11) (* 0.6385 x12) (* 0.6385 x21) (* -0.7562 x22)) z))

;(minimize (+ (* 0.3007 x11) (* 0.6385 x12) (* 0.6385 x21) (* -0.7562 x22)))

(minimize (+ (* 0.3007 x11) (* 0.6385 x12) (* 0.6385 x21) (* -0.7562 x22)))
(check-sat)
(get-objectives)

(get-model)

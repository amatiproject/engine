#lang racket

(require "Geometry-Engine.rkt")

(arcthickness 1)
(mirroring #t)
(elaboration #t)

(coded-by "Harry Mairson")
(title "Violoncello by Domenico Montagnana (const. François Denis)")

; Cello by Domenico Montagnana

(define (Montagnana)
 
; LAYOUT OF THE AREA on which the curves are drawn...

(let* ((aaprime 434)
       (X (label "X" origin))
       (A (label "A" (xshift X (- (/ aaprime 2)))))
       (App (label "A''" (yshift X (- (/ aaprime 2)))))
       (p (label "p" (bottom (intersect (vertical A)
                                        (circlefrom A App)))))
       (pp (label "p'" (mirror p)))
       (Q (label "Q" (yshift X (* (distance A p) (/ 7 5)))))
       (Z (label "Z" (yshift X (* (distance A p) (/ 21 40)))))
       (N (label "N" (yshift X (* (distance X Z) (/ 5 8)))))
       (O (label "O" (yshift X (* (distance X Q) (/ 2 3)))))
       (M (label "M" (yshift X (- (/ (distance A p) 2)))))
       (P (label "P" (midpoint p (mirror p))))
       (q (label "q" (at (xshift A (/ (distance A X) 5)) Q)))
       (qp (label "q'" (mirror q)))
       (a (label "a" (xshift A (* (distance A X) (/ 2 5)))))
       (b (label "b" (xshift Z (- (* (distance a X) (/ 5 12))))))
       (e (label "e" (xshift (at b N) (- (* (xdistance  A b) (/ 5 12))))))
       (g (label "g" (at q Z)))
       (d (label "d" (at q X)))
       (c (label "c" (midpoint A d)))
       (h (label "h" (midpoint g (at e Z))))
              
; Lower bouts
       (R1lower (circlefrom Z P))
       (R2lower (lower-left-flank (vertical p) R1lower (distance P M)))
       (R3lower (left-flush R2lower (distance X P)))
       (R4lower (lower-corner R3lower (/ (distance X N) 2) c))
       (lower-curve (make-curve P c (list R1lower R2lower R3lower R4lower)))

; Upper bouts
       (XZ4 (/ (distance X Z) 4))
                            
       (R1upper (circlefrom N Q))
       (R2upper (upper-left-flank (vertical q) R1upper (distance O Q)))
       (k (label "k" (yshift (midpoint g h) XZ4)))
       (R4upper (circle (left (intersect (circle g XZ4) (circle k XZ4))) XZ4))
       (R3upper (first (tangent R4upper R2upper)))
       (upper-curve (make-curve Q g (list R1upper R2upper R3upper R4upper)))
       
; Middle bouts
       (R1middle (circlefrom (xshift e (- (distance X Z))) e))
       (R2middle (middle-top-corner R1middle XZ4 h))
       (R3middle (middle-bottom-corner R1middle (/ (distance X N) 2) d))
       (middle-curve (make-curve g c (list R2middle R1middle R3middle))))
       
  (list X A App p pp Q Z N O M P q qp a b e g d c h k
        (map vertical (list p a b X q))
        (map horizontal (list X Q P b e M O))
        R1lower R2lower R3lower R4lower
        R1upper R2upper R3upper R4upper
        R1middle R2middle R3middle
        upper-curve middle-curve lower-curve
        )))

(sketch (Montagnana))

(end-drawing)
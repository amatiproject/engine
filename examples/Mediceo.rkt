#lang racket

(require "Geometry-Engine.rkt")

(elaboration #t)
(mirroring #t)
(arcthickness 2)
(arccolor "blue")

(coded-by "Harry Mairson")
(title "Violoncello by Antonio Stradivari ['Mediceo', 1690]")

(define (Mediceo)
(let* (
       
       (length 782.5)
       (width (* length (: 7 5)))
       
       (X (label "X" origin))
       (Q (label "Q" (yshift X width)))
       (P (label "P" (yshift Q (- length))))
       (Z (label "Z" (pointfrom X Q (: 2 3))))
       
       (x (label "x" (xshift X (- (/ width 2)))))
       (xp (label "x'" (mirror x)))
       
       (p (label "p" (at x P)))
       (pp (label "p'" (mirror p)))
       (maxlowerwidth (distance p pp))
       
       (q (label "q" (pointfrom (at p Q) Q (: 2 7))))
       (qp (label "q'" (mirror q)))
       (maxupperwidth (distance q qp))

       (N (label "N" (intersect (line p qp) (line pp q))))   ; diagonal form: note ZN:ZX = 3:5
       
       (O (label "O" (pointfrom Q N (: 4 5))))
       (M (label "M" (pointfrom X P (: 1 1))))

       (e (label "e" (pointfrom N (at q N) (: 2 1))))
       (ep (label "e'" (mirror e)))
       
       (h (label "h" (pointfrom (at e Z) (at q Z) (: 1 2))))
       (hp (label "h'" (mirror h)))
       (d (label "d" (xshift (at p X) (xdistance q e))))
       (dp (label "d'" (mirror d)))
       (g (label "g" (xshift (at p Z) (xdistance q e))))
       (gp (label "g'" (mirror g)))
       (c (label "c" (midpoint x d)) )
       (cp (label "c'" (mirror c)))

; middle bout       
       
       (fradius (* (distance Z Q) (: 4 1)))
       (dradius (/ (distance N X) 2))
       (hradius (/ (distance N Z) 2))
       (gradius (* (distance N Z) (: 5 3)))
       (cradius gradius)  ; !!!
       
       (R1middle (circle (xshift e (- fradius)) fradius))
       (R2middle (middle-top-corner R1middle hradius h))
       (R3middle (middle-bottom-corner R1middle dradius d))
       
; upper bout
       
       (R1upper (circlefrom Z Q))
       (R2upper (upper-left-flank (vertical q) R1upper (distance O Q)))
       (R3upper (left-flush R2upper (* (distance Q Z) (: 5 2))))
       (R4upper (upper-corner R3upper gradius g))

; lower bout
       (R1lower (circlefrom (midpoint Z O) P))
       (R2lower (lower-left-flank (vertical p) R1lower (* (distance N P) (/ 5 7) (/ 1 2))))
       (R3lower (left-flush R2lower  (/ (- maxlowerwidth (/ (xdistance p q) 3)) 2)))
       (k (label "k" (yshift c (- (/ (distance X Z) 4)))))
       (R4lower (circle (left (intersect (circle c cradius) (circle k cradius))) cradius))
       (tangentline (first (tangent R4lower R3lower)))

       )
       
  (list  (make-curve Q g (list R1upper R2upper R3upper R4upper))
         (make-curve P c (list R1lower R2lower R3lower tangentline R4lower))
         (make-curve g c (list R2middle R1middle R3middle))
         
         R1lower R2lower R3lower R4lower tangentline
         R1upper R2upper R3upper R4upper
         R1middle R2middle R3middle
         
         X Q P Z x xp p pp q qp N O M e ep g gp h hp c cp d dp k (label "k'" (mirror k))
  
         (map vertical (list X p pp q qp e ep))
         (map horizontal (list P Q Z N X O M))
         (line p qp) (line pp q) tangentline 
        )))


(sketch (Mediceo))

(end-drawing)
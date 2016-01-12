#lang racket

(require "Geometry-Engine.rkt")

(elaboration #t)
(mirroring #t)
(arcthickness 1)
(arccolor "blue")

(coded-by "Harry Mairson")
(title "Violoncello by Antonio Stradivari ['Cristiani', 1700]")

(define (Cristiani)
(let* ((length 758)   ; average of front=766mm and back=770, minus 10mm
       (alpha (/ 412. 758))  ; ratio of body stop to length of mold
       (width (* length (: 7 5)))
       
       (X (label "X" origin))
       (Q (label "Q" (yshift X (* length (: 4 3)))))
       (P (label "P" (yshift Q (- length))))
       (Z (label "Z" (pointfrom X Q (: 2 3)))) 
       (O (label "O" (pointfrom Q X (: 1 2))))
       (M (label "M" (pointfrom X P (: 4 5))))
       (N (label "N" (pointfrom Z X (: 2 3))))
;       (N (intersect (line p qp) (line pp q)))   ; diagonal form
       
       (p (label "p" (at (xshift X (- (/ width 2))) P)))
       (pp (label "p'" (mirror p)))
       (q (label "q" (pointfrom (at p Q) Q (: 2 7))))
       (qp (label "q'" (mirror q)))
       
       (e (label "e" (pointfrom N (at q N) (: 2 1))))
       (ep (label "e'" (mirror e)))
       (h (label "h" (pointfrom (at e Z) (at q Z) (: 1 1))))
       (hp (label "h'" (mirror h)))
       (g (label "g" (pointfrom (at e Z) (at q Z) (: 3 1))))
       (gp (label "g'" (mirror g)))
       (c (label "c" (pointfrom (at e X) (at p X) (: 3 1))))
       (cp (label "c'" (mirror c)))
       (d (label "d" (at g X)))
       (dp (label "d'" (mirror d)))

       ; ff-holes (vertical lines marking widths)
       (a (vertical (xshift Q (- (xdistance p q)))))
       (b (vertical (xshift Q (* (distance Q q) (: 3 1)))))
       
       (gradius (/ (distance N Z) 2))
       (hradius (* (: 4 1) gradius))
       (cradius (/ (distance X N) 2))
       (dradius cradius)

; middle bout       
       (R1middle (circlefrom (xshift e (- (distance X Z))) e))
       (R2middle (middle-top-corner R1middle hradius h))       
       (R3middle (middle-bottom-corner R1middle dradius d))
       (middle-curve (make-curve g c (list R2middle R1middle R3middle)))
       
; lower bout
       (R1lower (circlefrom Z P))
       (R2lower (lower-left-flank (vertical p) R1lower (* (distance N P) (: 5 2) (/ 1 2))))
       (R3lower (left-flush R2lower (distance Z Q)))
       (R4lower (lower-corner R3lower cradius c))
       (lower-curve (make-curve c P (list R4lower R3lower R2lower R1lower)))
       
; upper bout
       (R1upper (circlefrom Z Q))
       (R2upper (upper-left-flank (vertical q) R1upper (distance O Q)))
       (R3upper (left-flush R2upper (* (distance Q Z) (: 5 2))))
       (R4upper (upper-corner R3upper gradius g))
       (upper-curve (make-curve Q g (list R1upper R2upper R3upper R4upper)))
         )
  (list a b X Q P Z N O M q qp p pp N e ep g gp h hp c cp d dp
        (map horizontal (list X Q P Z N O M)) 
        (map vertical (list p P pp e ep q qp)) 

        R1lower R2lower R3lower R4lower
        R1middle R2middle R3middle
        R1upper R2upper R3upper R4upper
        lower-curve middle-curve upper-curve)
        ))


(sketch (Cristiani))

(end-drawing)
#lang racket

(require "Geometry-Engine.rkt")

(mirroring #t)

(coded-by "Harry Mairson")
(title "Violoncello by Josephus 'Filius Andrea' Guarneri (const. François Denis)")

; CELLO BY JOSEPH FILIUS ANDREA GUARNERI

(define (Guarneri)

; LAYOUT OF THE AREA on which the curves are drawn...

(let* ((aaprime 427)
       (X (label "X" origin))
       (A (label "A" (xshift X (- (/ aaprime 2)))))
       (Q (label "Q" (yshift X aaprime)))
       (P (label "P" (yshift X (* (distance X Q) (/ -3 4)))))
       (Z (label "Z" (pointfrom Q X geometric)))
       (N (label "N" (yshift X (* (distance Z X) (/ 5 8)))))
       (M (label "M" (midpoint P X)))
       (O (label "O" (midpoint Z Q)))
       (a (label "a" (pointfrom X A geometric)))
       (q (label "q" (at (midpoint a A) Q)))
       (qp (label "q'" (mirror q)))
       (p (label "p" (at A P)))
       (pp (label "p'" (mirror p)))
       (b (label "b" (at (xshift a (distance A a)) Z)))
       (e (label "e" (xshift N (/ (distance q (mirror q)) -3))))
       (d (label "d" (xshift A (/ (xdistance e A) 2))))
       (g (label "g" (xshift (at A Z) (/ (xdistance e A) 2))))
       (c (label "c" (xshift A (* (xdistance e A) (/ 1 5)))))
       (h (label "h" (xshift (at e Z) (* (xdistance e A) (/ -1 5)))))

; bottom bouts
       (R1lower (circlefrom N P))
       (R2lower (lower-left-flank (vertical p) R1lower (distance M P)))
       (R3lower (left-flush R2lower (distance N M)))
       (R4lower (lower-corner R3lower (/ (distance X N) 2) c))
       (lower-curve (make-curve P c (list R1lower R2lower R3lower R4lower)))
                                                          
; upper bouts                             
       (R1upper (circlefrom N Q))
       (R2upper (upper-left-flank (vertical q) R1upper (distance O Q)))
       (R3upper (left-flush R2upper (distance (center (mirrorcircle R2upper)) q)))
       (R4upper (upper-corner R3upper (/ (distance X N) 2) g))
       (upper-curve (make-curve Q g (list R1upper R2upper R3upper R4upper)))
                             
; middle bouts
       (R1middle (circlefrom (xshift e (- (distance X Z))) e))
       (R2middle (middle-bottom-corner R1middle (/ (distance X N) 2) d))
       (R3middle (middle-top-corner R1middle (/ (distance Z N) 2) h))
       (middle-curve (make-curve c g (list R2middle R1middle R3middle)))
    )
  (list X A Q P Z N M O a q qp p pp b e d g c h 
        (map vertical (list A b e))  
        (map horizontal (list P X N Z Q))

        R1upper R2upper R3upper R4upper
        R1middle R2middle R3middle
        R1lower R2lower R3lower R4lower
        upper-curve middle-curve lower-curve    
         )))

(sketch (Guarneri))

(end-drawing)
#lang racket

(require "Geometry-Engine.rkt")

(elaboration #f)
(mirroring #t)
(arcthickness 2)
(arccolor "yellow")

(title "Viola by Andrea Guarneri (const. François Denis)")
(coded-by "Harry Mairson")

(define (Guarneri-viola)
(let* ((width 237)
       
       ; FRAMEWORK
       
       (X (label "X" origin))
       (A (label "A" (xshift X (/ width -2))))
       (Ap (label "A'" (mirror A)))
       (Q (label "Q" (yshift X width)))
       (Z (label "Z" (pointfrom Q X harmonic)))
       (P (label "P" (at X (south (circle Ap (/ (distance Ap (at A Q)) 2))))))
       (p (label "p" (intersect (horizontal P) (vertical A))))
       (pp (label "p'" (mirror p)))
       (N (label "N" (pointfrom X Z (: 3 2))))
       (O (label "O" (yshift Z (distance N X))))
       (M (label "M" (midpoint X P)))
       
       (a (label "a" (at (pointfrom p P (: 3 5)) A)))
       (b (label "b" (at (pointfrom X a (: 1 2)) Z)))
       (e (label "e" (at (pointfrom b (at A Z) (: 3 4)) N)))
       (q (label "q" (pointfrom (at b Q) (at e Q) (/ 7 4))))
       (qp (label "q'" (mirror q)))

       (d (label "d" (midpoint (at e X) (at p X))))
       (c (label "c" (midpoint d (at p X))))
       (g (label "g" (at d Z)))
       (h (label "h" (midpoint g (at e Z))))
       (foo (write (list 'length (distance P Q) 
                         'upper (distance q qp) 
                         'middle (distance e (mirror e)) 
                         'lower (distance p pp))))
       
       ; LOWER BOUT
       (R1lower (circlefrom O P))
       (R2lower (lower-left-flank (vertical p) R1lower (distance M P) ))
       (R3lower (left-flush R2lower (distance X A)))
       (R4lower (lower-corner R3lower (/ (distance X N) 2) c))
       (lower-curve (make-curve P c (list R1lower R2lower R3lower R4lower)))
       
       ; MIDDLE BOUT
       (R1middle (circlefrom (xshift e (- (distance X A))) e))
       (R2middle (middle-bottom-corner R1middle (/ (distance X N) 2) d))
       (R3middle (middle-top-corner R1middle (/ (distance Z N) 2) h))
       (middle-curve (make-curve c g (list R2middle R1middle R3middle)))
       
       ; UPPER BOUT
       (R1upper (circlefrom N Q))
       (R2upper (upper-left-flank (vertical q) R1upper (distance O Q)))
       (R3upper (left-flush R2upper (xdistance q Q)))
       (R4upper (upper-corner R3upper (/ (distance X N) 2) g))
       (upper-curve (make-curve Q g (list R1upper R2upper R3upper R4upper)))
       
         )
       
  (list X A Ap Q Z P p pp N O M a b e q qp
        d c g h 
        (map vertical (list q p e a b)) 
        (map horizontal (list Q O Z N X M P))
                
        R1lower R2lower R3lower R4lower
        R2middle R1middle R3middle
        R1upper R2upper R3upper R4upper
        lower-curve middle-curve upper-curve
        )

        ))

(sketch (Guarneri-viola))

(end-drawing)
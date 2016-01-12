#lang racket

(require "Geometry-Engine.rkt")

(elaboration #t)
(mirroring #t)
(arcthickness 1)
(arccolor "blue")

(title "Violoncello by Antonio Stradivari ['Countess of Stanlein', 1707]")

; This design, as programmed here, comes from a modification of the Mediceo design

(define (Stanlein)         ; parameters given for mold
(let* (
       (length 745)
       (bodystop 400)      ; not used in the proportional construction
                 
;       (rho 0.9354901578639642)  ; ratio of bodystop (400mm) to XQ in a 745mm mold (and invariant! ...)
              
       (MediceoLength 733)  ; length of rescaled Mediceo framework, extended to Stanlein
       
       (boutshift (- MediceoLength length))   ;  -12 when length=745 and MediceoLength=733
       (new (lambda (o) (ymove o boutshift)))
       
       (width (* MediceoLength (: 7 5)))
       
       (X (label "X" origin))
       (Q (label "Q" (yshift X width)))
       (P (label "P" (yshift Q (- MediceoLength))))
       (Z (label "Z" (pointfrom X Q (: 2 3))))
       
       (x (label "x" (xshift X (- (/ width 2)))))
       (xp (label "x'" (mirror x)))
       
       (p (label "p" (intersect (vertical x) (horizontal P))))
       (pp (label "p'" (mirror p)))
       (maxlowerwidth (distance p pp))
       
       (q (label "q" (pointfrom (intersect (horizontal Q) (vertical p))
                                Q
                                (: 2 7))))
       (qp (label "q'" (mirror q)))
       (maxupperwidth (distance q qp))

       (N (label "N" (intersect (line p qp) (line pp q))))   ; diagonal form: note ZN:ZX = 3:5
       
       (O (label "O" (pointfrom Q N (: 4 5))))
       (M (label "M" (pointfrom X P (: 1 1))))
       
       (e (label "e" (pointfrom N (intersect (horizontal N) (vertical q)) (: 2 1))))
       (ep (label "e'" (mirror e)))
       
; CHANGES TO THE LOWER BOUT, ETC.
       

       
       (newN (label "N" (intersect (line q (new pp)) (line qp (new p)))))
       (newe (label "e" (intersect (horizontal newN) (vertical e))))
       (newep (label "e'" (mirror newe)))
       
; middle bout  
       
       (h (label "h" (pointfrom (intersect (horizontal Z) (vertical newe)) 
                                (intersect (horizontal Z) (vertical q))
                                (: 1 2.5))))
       (hp (label "h'" (mirror h)))
       
       ; new d
       (d (label "d" (xshift (intersect (horizontal X) (vertical newe))
                              (* (distance (intersect (horizontal X) (vertical newe)) X) (: -2 3)))))
       (dp (label "d'" (mirror d)))
       
       ;revised g from Mediceo
       (g (label "g" (pointfrom (intersect (horizontal Z) (vertical p))
                                (intersect (horizontal Z) (vertical newe))
                                (: 4 3))))
       (gp (label "g'" (mirror g)))
       
       (c (label "c" (pointfrom x
                                (intersect (horizontal X) (vertical newe))
                                .3)))
       (cp (label "c'" (mirror c)))
       
       ; a new, larger curve for the upper part of the bout

;       (midverybigcircleL (circlefrom (xshift newe (- (* 0.75 (/ 5 4) (distance Q X)))) newe))
       (midverybigcircleL (circlefrom (xshift newe (- (* (: 4 1) (distance Q X)))) newe))
       (midverybigcircleR (mirrorcircle midverybigcircleL))

       ; the two curves at the bottom and top of the middle bout...
       (dradius (xdistance p q))
       (dcircleL (circle (top (intersect (circle c dradius) (circle d dradius))) dradius))
       (dcircleR (mirrorcircle dcircleL))
       
       (bighradius (* (distance N Z) (: 5 3)))
;       (bighradius (/ (distance newN Z) 2))
       (bighcircleL (lower-circle (inscribepoint midverybigcircleL h bighradius)))
       (hradius (/ (* (distance newN Z) (: 3 1)) 2))
       (hcircleL (circle (pointfrom h (center bighcircleL) (/ hradius (radius bighcircleL))) hradius))
       (hcircleR (mirrorcircle hcircleL))
       
       ; now fit the curve for the main lower part of the middle bout
       ; by fitting it to midverybigcircleL and dcircleL
       
       (fradius (* (distance Z Q) (: 3 2)))
       (fcircleL (circle (right (intersect (circle (center dcircleL)
                                                   (- fradius dradius))
                                           (circle (center midverybigcircleL)
                                                   (- (radius midverybigcircleL) fradius))))
                         fradius))
       (fcircleR (mirrorcircle fcircleL))
       
       (gradius (* (distance N Z) (: 5 3)))
       (cradius (/ (* (distance x (intersect (horizontal X) (vertical newe))) (: 2 1)) 2))
       
; upper bout
       
;       (uppercircle (circlefrom (new N) Q))
       (uppercircle (circlefrom Z Q))

       ; new to Stanlein
       (uppermidcircleradius (/ (distance Q Z) 2))
       (uppermidcircleL
         (inscribeinside-circle-line top uppercircle (vertical q) uppermidcircleradius))
       (uppermidcircleR (mirrorcircle uppermidcircleL))

       ; new to Stanlein
       (upperlowcircleradius (* (distance Q newN) (: 5 2) 1 ))  ; was (: 5 2)
       (upperlowcircleL
         (inscribeoutside-circle-line bottom uppermidcircleL (vertical q) upperlowcircleradius))
       (upperlowcircleR (mirrorcircle upperlowcircleL))
       
       (upperreversecircleL
        (upper-circle (reverse-curve upperlowcircleL (+ gradius (radius upperlowcircleL)) g)))
       (upperreversecircleR (mirrorcircle upperreversecircleL))

; lower bout
;       (lowercircle (circlefrom (midpoint Z O) (new P)))
       (lowercircle (circlefrom Q (new P)))
       
       (lowermidcircleradius (* (distance N P) (/ 3 4) (/ 1 2)))
       (lowermidcircleL 
         (inscribeinside-circle-line bottom lowercircle (vertical p) lowermidcircleradius))
       (lowermidcircleR (mirrorcircle lowermidcircleL))
       (lowertopcircleradius (/ (- maxlowerwidth (/ (xdistance p q) 3)) 2))
       
;       (k (label "k" (yshift c (- (xdistance q newe)))))
;       (k (ymove k (- boutshift StandardBoutshift)))
       
;       (lowertopcircleL
;        (circle (bottom (intersect (circle k lowertopcircleradius) 
;                                   (vertical (xshift (intersect (vertical p)
;                                                                (horizontal (center lowermidcircleL)))
;                                                     lowertopcircleradius))))
;                lowertopcircleradius))

; !!!       
       (lowertopcircleL (circle (xshift (west lowermidcircleL) lowertopcircleradius)
                                lowertopcircleradius))
                
                
       (lowertopcircleR (mirrorcircle lowertopcircleL))

       (k2 (label "k" (yshift c (- (/ (distance X Z) 4)))))
       (lowerreversecurveL
         (circle (left (intersect (circle c cradius) (circle k2 cradius))) cradius))

       (tangentline (first (tangent lowerreversecurveL lowertopcircleL)))
        
       (lowerreversecurveR (mirrorcircle lowerreversecurveL)) 
       
       (totallength (write (list '(length of mold) (distance (new P) Q))))
       
       )
       
  (list  
         (make-curve Q g (list uppercircle uppermidcircleL upperlowcircleL upperreversecircleL))
         (make-curve g c (list hcircleL bighcircleL midverybigcircleL fcircleL dcircleL))
         (make-curve c ;(right (intersect dcircleL lowerreversecurveL)) 
                     P 
                     (list lowerreversecurveL tangentline lowertopcircleL lowermidcircleL lowercircle))
         
         X Q Z x xp q qp newN O M newe newep g gp h hp c cp d dp
         (label "P" (new P)) (label "p" (new p)) (label "p'" (new pp))
         (map vertical (list X p pp q qp e ep))
         (map horizontal (list P (new P) Q Z newN X O M))
         fcircleL ; fcircleR
         dcircleL ; dcircleR
         hcircleL ; hcircleR
         
         lowercircle lowermidcircleL ; lowermidcircleR
         lowermidcircleL ; lowermidcircleR
         lowertopcircleL ; lowertopcircleR 
         lowerreversecurveL ; lowerreversecurveR
         
         uppercircle uppermidcircleL ; uppermidcircleR
         uppermidcircleL ; uppermidcircleR
         upperlowcircleL ; upperlowcircleR
         upperreversecircleL ; upperreversecircleR
         
;         (line p qp) (line pp q)
         (line (new p) qp) (line (new pp) q)
         tangentline 
;         k (label "k'" (mirror k))
         k2 (label "k'" (mirror k2))
         
         (vertical (point (- (xdistance p q)) 0))
         
         bighcircleL midverybigcircleL
         
         (circle Q bodystop)   ; to see arc of bodystop

        )))


(sketch (Stanlein))

(end-drawing)
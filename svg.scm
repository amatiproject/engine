(load "mergesort.scm")

(define reduce
  (lambda (f L n)
    (if (null? L)
        n
        (f (car L) (reduce f (cdr L) n)))))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define filter
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
          (else (filter pred (cdr lst))))))

(define (flatten-once lst) 
  (apply append lst))

(define (radian->degree rad) (* (/ 180 pi) rad))
(define (push lst val) (append lst (list val)))
(define concat string-append)
(define (concat-list lst) (reduce concat lst ""))
(define (stringify ele) (if (string? ele) ele (number->string ele)))
(define space " ")
(define slash "/")
(define blank "")
(define equal "=")
(define linebreak "\n")
(define (end-tag tag) (tagify (concat slash tag)))
(define nil '())
(define empty-body nil)
(define (tagify str) (concat "<" str ">"))
(define (quoteify str) (concat "\"" str "\""))
(define (key att) (car att))
(define (val att) (cadr att))
(define (attrs xml) (cadr xml))
(define (tag xml) (car xml))
(define (body xml) (caddr xml))
(define (xml? xml) (list? xml))
(define (filter-empty lst) (filter (lambda (x) (not (null? x))) lst))

(define (get-att-values att-key xml) (map val (filter (lambda (x) (eq? att-key (key x))) (attrs xml))))
(define (print-att att) (concat (key att) equal (quoteify (val att))))
(define (print-attrs attrs)
  (concat-list (map (lambda (att) (concat space (print-att att))) attrs)))
(define (print-body body)
  (concat-list (map (lambda (xml) (if (xml? xml) (print-xml xml) xml)) body)))

(define (print-xml xml)
  (let ((tag (tag xml))
        (attrs (attrs xml))
        (body (body xml)))
    (concat
     (tagify
      (concat tag
              (print-attrs attrs)))
     (print-body body)
     (end-tag tag))))


(define xml-version "1.0")

(define (make-xml tag attrs body)
  (list tag attrs body))

(define (xml-att-append xml val)
	(make-xml (tag xml) (push (attrs xml) val) (body xml) val))
(define (xml-body-append xml val)
	(make-xml (tag xml) (attrs xml) (push (body xml) val)))
(define (make-att key val) (list (stringify key) (stringify val)))
(define (att? att) (cond
                     ((atom? att) #f)
                     ((null? att) #f)
                     ((and (atom? (car att)) (atom? (cadr att)) (eq? (length att) 2)) #t)
                     (else #f)))

(define (attrs? attrs)
  (cond
    ((atom? attrs) #f)
    ((null? attrs) #f)
    ((null? (filter (lambda (att) (not (att? att))) attrs)) #t)
    (else #f)))

(define (listify ele)
  (if (list? ele) ele (list ele)))

(define (not-null lst) (not (null? lst)))

(define (last lst)
  (car (reverse lst)))

(define (no-last lst)
  (reverse (cdr (reverse lst))))



(define (make-attrs . attrs)
  (let ((last-ele (last attrs))
        (firstn-ele (no-last attrs)))
    (filter not-null (if (attrs? last-ele)
                         (append firstn-ele last-ele)
                         (reverse (cons last-ele (reverse firstn-ele)))))))

(define (make-body . body) body)

(define print-svg print-xml)

(define (svg-circle cx cy r . opt)
  (make-xml "circle"
            (make-attrs
             (make-att "cx" cx)
             (make-att "cy" cy)
             (make-att "r" r)
             opt)
            nil))

(define (svg-line x1 y1 x2 y2 . opt)
  (make-xml "line"
            (make-attrs
             (make-att "x1" x1)
             (make-att "y1" y1)
             (make-att "x2" x2)
             (make-att "y2" y2)
             opt)
            nil))

(define (svg-ellipse cx cy rx ry . opt)
  (make-xml "ellipse"
            (make-attrs
             (make-att "cx" cx)
             (make-att "cy" cy)
             (make-att "rx" rx)
             (make-att "ry" ry)
             opt)
            nil))

(define (svg-text x y content . opt)
  (make-xml "text"
            (make-attrs
             (make-att "x" x)
             (make-att "y" y)
             opt)
            (make-body (stringify content))))

(define (svg-rect x y w h . opt)
  (make-xml "rect"
            (make-attrs
             (make-att "x" x)
             (make-att "y" y)
             (width w)
             (height h)
             opt)
            nil))

(define (svg-path d . opt)
	(make-xml "path"
		(make-attrs
			(desc d)
			opt)
		nil))

(define (svg-dc-arc x y w h start-radians end-radians . opt)
	(make-xml "path"
		(make-attrs
		(desc (concat "M" space (stringify x) space (stringify y) space "A" space (stringify (radian->degree start-radians)) space (stringify (radian->degree end-radians)) space "0" space "0" space "0" space (stringify (+ x w)) space (stringify (+ y h))))
		(make-att "fill" "transparent")
		opt)
		nil)) 

(define (svg-arc x1 y1 rx ry x2 y2 sweep . opt)
	(make-xml "path"
		(make-attrs
		(desc (concat "M" space (stringify x1) space (stringify y1) space "A" space (stringify rx) space (stringify ry) space "0" space "0" space (if sweep "1" "0") space (stringify x2) space (stringify y2)))
		(make-att "fill" "transparent")
		opt)
		nil)) 


(define (height amt) (make-att "height" amt))
(define (width amt) (make-att "width" amt))
(define (version amt) (make-att "version" amt))
(define (desc val) (make-att "d" val))


(define (viewBox min-x min-y width height) (make-att "viewBox" (concat min-x space min-y space width space height)))
(define (svg height-val width-val)
	(make-xml "svg"
		(make-attrs 
			(height height-val)
			(width width-val))
		empty-body))

(define svg-example 
  (make-xml "svg"
            (make-attrs
             (height "500")
             (width "500")
             (viewBox "-650" "-400" "2000" "2500")
             (version "1.1"))
            (make-body
             (svg-circle "250" "250" "50")
             (svg-circle "100" "100" "25")
             (svg-circle "150" "150" "10")
             (svg-line "100" "100" "200" "200" (make-att "stroke" "black"))
             (svg-ellipse "323" "123" "55" "21")
             (svg-text "10" "12" "eden"))))



;(display (print-xml svg-example))


;(display (print-xml svg))
;(display (print-xml (xml-body-append svg (svg-text "11" "11" "poop"))))
;(display (print-xml (xml-att-append svg (version "1.2"))))
;(display (print-xml (xml-att-append (svg 100 200) (version "1.2"))))



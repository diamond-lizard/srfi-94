(define (type-error procedure . args) (apply error procedure args))
;@
(define (integer-expt x1 x2)
  (cond ((and (exact? x1) (integer? x1)
          (exact? x2) (integer? x2)
          (not (and (not (<= -1 x1 1)) (negative? x2))))
     (expt x1 x2))
    (else (type-error 'integer-expt x1 x2))))
;@
(define (integer-log base k)
  (define (ilog m b k)
    (cond ((< k b) k)
      (else
       (set! n (+ n m))
       (let ((q (ilog (+ m m) (* b b) (quotient k b))))
         (cond ((< q b) q)
           (else (set! n (+ m n))
             (quotient q b)))))))
  (define n 1)
  (define (eigt? k j) (and (exact? k) (integer? k) (> k j)))
  (cond ((not (and (eigt? base 1) (eigt? k 0)))
     (type-error 'integer-log base k))
    ((< k base) 0)
    (else (ilog 1 base (quotient k base)) n)))

;;;; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/isqrt/isqrt.txt
;;; Akira Kurihara
;;; School of Mathematics
;;; Japan Women's University
;@
(define integer-sqrt
  (let ((table '#(0
          1 1 1
          2 2 2 2 2
          3 3 3 3 3 3 3
          4 4 4 4 4 4 4 4 4))
    (square (lambda (x) (* x x))))
    (lambda (n)
      (define (isqrt n)
    (if (> n 24)
        (let* ((len/4 (quotient (- (integer-length n) 1) 4))
           (top (isqrt (ash n (* -2 len/4))))
           (init (ash top len/4))
           (q (quotient n init))
           (iter (quotient (+ init q) 2)))
          (cond ((odd? q) iter)
            ((< (remainder n init) (square (- iter init))) (- iter 1))
            (else iter)))
        (vector-ref table n)))
      (if (and (exact? n) (integer? n) (not (negative? n)))
      (isqrt n)
      (type-error 'integer-sqrt n)))))

(define (must-be-exact-integer2 name proc)
  (lambda (n1 n2)
    (if (and (integer? n1) (integer? n2) (exact? n1) (exact? n2))
    (proc n1 n2)
    (type-error name n1 n2))))
;@
(define quotient  (must-be-exact-integer2 'quotient quotient))
(define remainder (must-be-exact-integer2 'remainder remainder))
(define modulo    (must-be-exact-integer2 'modulo modulo))

;;;; real-only functions
;@
(define (quo x1 x2) (truncate (/ x1 x2)))
(define (rem x1 x2) (- x1 (* x2 (quo x1 x2))))
(define (mod x1 x2) (- x1 (* x2 (floor (/ x1 x2)))))

(define (must-be-real name proc)
  (lambda (x1)
    (if (real? x1) (proc x1) (type-error name x1))))
(define (must-be-real+ name proc)
  (lambda (x1)
    (if (and (real? x1) (>= x1 0))
    (proc x1)
    (type-error name x1))))
(define (must-be-real-1+1 name proc)
  (lambda (x1)
    (if (and (real? x1) (<= -1 x1 1))
    (proc x1)
    (type-error name x1))))
;@
(define ln log)
(define abs       (must-be-real 'abs abs))
(define real-sin  (must-be-real 'real-sin sin))
(define real-cos  (must-be-real 'real-cos cos))
(define real-tan  (must-be-real 'real-tan tan))
(define real-exp  (must-be-real 'real-exp exp))
(define real-ln   (must-be-real+ 'ln ln))
(define real-sqrt (must-be-real+ 'real-sqrt sqrt))
(define real-asin (must-be-real-1+1 'real-asin asin))
(define real-acos (must-be-real-1+1 'real-acos acos))

(define (must-be-real2 name proc)
  (lambda (x1 x2)
    (if (and (real? x1) (real? x2)) (proc x1 x2) (type-error name x1 x2))))
;@
(define make-rectangular (must-be-real2 'make-rectangular make-rectangular))
(define make-polar       (must-be-real2 'make-polar make-polar))

;@
(define real-log
  (lambda (base x)
    (if (and (real? x) (positive? x)
         (real? base) (positive? base))
    (/ (ln x) (ln base))
    (type-error 'real-log base x))))
;@
(define (real-expt x1 x2)
  (cond ((and (real? x1)
          (real? x2)
          (or (not (negative? x1)) (integer? x2)))
     (expt x1 x2))
    (else (type-error 'real-expt x1 x2))))

;@
(define real-atan
  (lambda (y . x)
    (if (and (real? y)
         (or (null? x)
         (and (= 1 (length x))
              (real? (car x)))))
    (apply atan y x)
    (apply type-error 'real-atan y x))))

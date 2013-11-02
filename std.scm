(define (not x) (if x #f #t))
(define (null? obj) (if (= obj '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip fn) (lambda (arg1 arg2) (fn arg2 arg1)))

(define (curry fn arg1) (lambda (arg) (apply fn (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry > 0))
(define negative? (curry < 0))
(define (odd? num) (= (mod num 2) 1))
(define even? (compose not odd?))

(define (foldr fn end lst)
  (if (null? lst)
    end
    (fn (car lst) (foldr fn end (cdr lst)))))
(define (foldl fn accum lst)
  (if (null? lst)
    accum
    (foldl fn (fn accum (car lst)) (cdr lst))))

(define (unfold fn init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold fn (fn init) pred))))

(define (range to) (unfold (curry + 1) 0 (curry <= to)))

(define (sum . lst) (foldl + 0 lst))
(define (product . lst) (foldl * 1 lst))
(define (and . lst) (foldl && #t lst))
(define (or . lst) (foldl || #f lst))

(define (max first . rest) (foldl (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (foldl (lambda (old new) (if (< old new) old new)) first rest))

(define (length lst) (foldl (lambda (len _) (+ len 1)) 0 lst))

(define (reverse lst) (foldl (flip cons) '() lst))

(define (zip xs ys)
  (zip* xs ys '()))

(define (zip* xs ys zipped)
  (if (or (null? xs) (null? ys))
    (reverse zipped)
    (zip* (cdr xs) (cdr ys) (cons (list (car xs) (car ys)) zipped))))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (member obj lst) (foldl (mem-helper (curry = obj) id) #f lst))
(define (assoc obj lst) (foldl (mem-helper (curry = obj) car) #f lst))

(define (map fn lst) (foldr (lambda (el acc) (cons (fn el) acc)) '() lst))

(define (filter pred lst) (foldr (lambda (el acc) (if (pred el) (cons el acc) acc)) '() lst))

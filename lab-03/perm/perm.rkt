#lang racket

(define (perm I O)
  (perm-aux (reverse I) (reverse O) (list)))

(define (perm-aux I O S)
  (cond
    [(and (eq? empty I) (eq? empty O) (eq? empty S)) #t]
    [(and (not (eq? empty S)) (not (eq? empty O)) (eq? (first O) (first S))) (perm-aux I (rest O) (rest S))]
    [(and (not (eq? empty I)) (not (eq? empty O)) (eq? (first I) (first O))) (perm-aux (rest I) (rest O) S)]
    [(and (not (eq? empty I))) (perm-aux (rest I) O (append (list (first I)) S))]
    [else #f]
  )
 )

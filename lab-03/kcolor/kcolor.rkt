#lang racket

(define-struct graph (vertices edges))


(define empty (make-graph '() '()))

(define (add-vertex g v) (
  cond
    [(not (list? (member v (graph-vertices g)))) (make-graph (append (graph-vertices g) (list v)) (graph-edges g))]
    [else g]
))


(define (add-edge g v1 v2)
  (cond
    [(nor
      (list? (member (list v1 v2) (graph-edges g)))
      (list? (member (list v2 v1) (graph-edges g))))
       (make-graph (graph-vertices g) (append (graph-edges g) (list (list v1 v2))))]
    [else g]
))

(define (neighbours g v) (remove* (list v) (flatten (filter (lambda (x) (member v x))(graph-edges g)))))

(define (add-vertices g v)
  (cond
    [(empty? v) g]
    [else (add-vertices (add-vertex g (car v)) (rest v))]
  )
 )

(define (add-edges g e)
  (cond
    [(empty? e) g]
    [else (add-edges (add-edge g (caar e) (cadar e)) (rest e))]
  )
 )
(define (parse input) (add-edges (add-vertices empty (map (lambda (x) (car x)) input)) (foldr append '() (map (lambda (x) (map (lambda (y) (list (first x) y)) (second x))) input))))


(define (get-degree g v) (length (neighbours g v)))
(define (sort-by-degree g) (sort (graph-vertices g) (lambda (x y) (> (get-degree g x) (get-degree g y)))))
(define (get-color colors) (first (filter (lambda (x) (not (member x (map (lambda (y) (cadr y)) colors)))) alphabet)))
(define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define (color g vertices colors)
  (cond
    [(empty? vertices) colors]
    [else (
           let ([vertices-to-draw
                 (filter (lambda (x) (nor (member x (neighbours g (car vertices))) (list? (member x (map (lambda (y) (car y)) colors))))) (graph-vertices g))])
            (let ([new-colors (append colors (map (lambda (x) (list x (get-color colors))) vertices-to-draw))])
              (color g (remove* vertices-to-draw vertices) new-colors)
              )
            )])
  )

(define (kcolor input n) (let ([g (parse input)])
                         (let ([g-colored (sort (color g (sort-by-degree g) '()) (lambda (x y) (< (index-of (graph-vertices g) (car x)) (index-of (graph-vertices g) (car y)))))])
                           (cond
                             [(<= (length (remove-duplicates (map (lambda (x) (cadr x)) g-colored))) n) g-colored]
                             [else #f])
                           )))

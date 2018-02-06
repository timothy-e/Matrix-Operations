;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Matrix Operations|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Table is a (listof (listof Num))
;; requires: each sublist has the same length

;; (mult-row num row) returns a list of all of the terms from
;;     'row' multiplied by 'num'
;; mult-row: Num (listof Num) -> (listof Num)
;; Examples:
(check-expect (mult-row 2 (list -1 0 1 2))
              (list -2 0 2 4))
(check-expect (mult-row 8 empty)
              empty)

(define (mult-row num row)
  (cond
    [(empty? row)
     empty]
    [else
     (cons (* (first row) num)
           (mult-row num (rest row)))]))


;; (mult-by num my-table) returns a Table made of all the terms from
;;     'my-table' multiplied by 'num'
;; mult-by: Num Table -> Table
;; Examples
(check-expect (mult-by 2 (list (list 2 3 4)
                               (list 1 4 7)
                               (list -4 -2 0)))
              (list (list 4 6 8)
                    (list 2 8 14)
                    (list -8 -4 0)))
(check-expect (mult-by 4 empty) empty)
(check-expect (mult-by 4 (list empty empty)) (list empty empty))

(define (mult-by num my-table)
  (cond
    [(empty? my-table)
     empty]
    [else
     (cons (mult-row num (first my-table))
           (mult-by num (rest my-table)))]))

;; Tests:
(check-expect (mult-by 0 (list (list 1 2 3 4 5)
                               (list -5 -4 -3 -2 -1)))
              (list (list 0 0 0 0 0)
                    (list 0 0 0 0 0)))
(check-expect (mult-by -1 (list (list -1 0 1)
                                (list -2 -1 0)
                                (list -3 -2 -1)
                                (list -4 -3 -2)))
              (list (list 1 0 -1)
                    (list 2 1 0)
                    (list 3 2 1)
                    (list 4 3 2)))


;; (get-from-row pos my-row) returns the nth term of the given list 'my-row'
;;     It produces false is the given term does not exist
;; get-from-row Nat (listof Num) -> (anyof Num false)
;; Examples
(check-expect (get-from-row 0 (list 4 5 6)) 4)
(check-expect (get-from-row 5 (list 4 5 6)) false)
(check-expect (get-from-row 2 empty) false)

(define (get-from-row n my-row)
  (cond
    [(empty? my-row) false]
    [(> n 0)
     (get-from-row (sub1 n) (rest my-row))]
    [else
     (first my-row)]))


;; (get-elem row col my-table) produces the element from the given
;;     table 'my-table' at the given 'row' and given column 'col'
;;     It produces false if the given position does not exist
;; get-elem: Nat Nat Table -> (anyof Num false)
;; Examples
(check-expect (get-elem 2 2 (list (list 1 2 3 4)
                                  (list 2 4 6 8)
                                  (list 3 6 9 12)
                                  (list 4 8 12 16)))
              9)
(check-expect (get-elem 1 2 empty) false)
(check-expect (get-elem 8 12 (list (list 1 2)
                                   (list 3 4)))
              false)
(check-expect (get-elem 0 1 (list empty empty empty)) false)

(define (get-elem row col my-table)
  (cond
    [(empty? my-table)
     false]
    [(> row 0)
     (get-elem (sub1 row) col (rest my-table))]
    [else
     (get-from-row col (first my-table))]))

;; Tests
(check-expect (get-elem 0 0 (list (list -4 -3 -2 -1)
                                  (list 0 1 2 3)))
              -4)
(check-expect (get-elem 0 0 (list (list 1))) 1)


;; (get-nth-term n row) returns the nth term of the given list 'row'
;; get-nth-term Nat (listof Num) -> Num
;; Examples
(check-expect (get-nth-term 2 (list 1 2 3 4)) 3)
(check-expect (get-nth-term 4 (list 1 2 3)) empty)
(check-expect (get-nth-term 2 empty) empty)

(define (get-nth-term n row)
  (cond
    [(empty? row) empty]
    [(> n 0)
     (get-nth-term (sub1 n) (rest row))]
    [else ;; n is the first term
     (first row)]))


;; (col n my-table) returns all numbers in the nth column of 'my-table'
;;     from top to bottom
;; col: Nat Table -> (listof Num)
;; Examples
(check-expect (col 1 empty) empty)
(check-expect (col 2 (list empty empty)) empty)
(check-expect (col 5 (list (list 1 2 3)
                           (list 4 5 6)
                           (list 7 8 9)))
              empty)
(check-expect (col 2 (list (list 1 2 3 4)
                           (list 2 4 6 8)
                           (list 3 6 9 12)
                           (list 4 8 12 16)))
              (list 3 6 9 12))

(define (col n my-table)
  (cond
    [(empty? my-table) empty]
    [(empty? (first my-table)) empty]
    [(empty? (get-nth-term n (first my-table))) empty]
    [else
     (cons (get-nth-term n (first my-table))
           (col n (rest my-table)))]))

;; Tests
(check-expect (col 1 (list (list 1 2 3 4)
                           (list 2 4 6 8)
                           (list 3 6 9 12)
                           (list 4 8 12 16)))
              (list 2 4 6 8))
(check-expect (col 0 (list (list 7))) (list 7))


;; (sum-row row-1 row-2) produces a list where each term in the list is the
;;     sum of the respective terms in 'row-1' and 'row-2'
;; sum-row: (listof Num) (listof Num) -> (listof Num)
;; requires: row-1 and row-2 must have the same length
;; Examples
(check-expect (sum-row empty empty) empty)
(check-expect (sum-row (list 1 2 3)
                       (list 7 8 9))
              (list 8 10 12))

(define (sum-row row-1 row-2)
  (cond
    [(empty? row-1) empty]
    [else
     (cons (+ (first row-1) (first row-2))
           (sum-row (rest row-1) (rest row-2)))]))
                   

;; (sum-tables table-1 table-2) produces a table where each cell is the
;;     sum of the respective cells of 'table-1' and 'table-2'
;; sum-tables: Table Table -> Table
;; requires: table-1 and table-2 must have the same dimensions
;; Examples
(check-expect (sum-tables (list (list 1 2 3)
                                (list 4 5 6)
                                (list -1 -2 -3))
                          (list (list 9 8 7)
                                (list 3 2 1)
                                (list 9 9 9)))
              (list (list 10 10 10)
                    (list 7 7 7)
                    (list 8 7 6)))
(check-expect (sum-tables empty empty) empty)
(check-expect (sum-tables (list empty empty)
                          (list empty empty))
              (list empty empty))

(define (sum-tables table-1 table-2)
  (cond
    [(empty? table-1) empty]
    [else
     (cons (sum-row (first table-1) (first table-2))
           (sum-tables (rest table-1) (rest table-2)))]))

;; Tests
(check-expect (sum-tables (list (list 0 0)
                                (list 0 0))
                          (list (list 0 0)
                                (list 0 1)))
              (list (list 0 0)
                    (list 0 1)))
(check-expect (sum-tables (list (list 1))
                          (list (list 2)))
              (list (list 3)))
(check-expect (sum-tables (list (list 4 5 6)
                                (list 7 8 9))
                          (list (list -7 -7 -7)
                                (list -7 -7 -7)))
              (list (list -3 -2 -1)
                    (list 0 1 2)))


;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

;; (mirror my-table) produces 'my-table' except with all of the elements
;;    in each row reversed
;; mirror: Table -> Table
;; Examples
(check-expect (mirror empty) empty)
(check-expect (mirror (list empty)) (list empty))
(check-expect (mirror '((1 2 3)))
              '((3 2 1)))
(check-expect (mirror '((5 4 5 4)
                        (3 6 9 12)))
              '((4 5 4 5)
                (12 9 6 3)))

(define (mirror my-table)
  (local
    ;; (reverse-row my-row) reverses 'my-row', but reversed
    ;; reverse-row: (listof Num) -> (listof Num)
    [(define (reverse-row my-row)
       (reverse-row/acc my-row empty))
     ;; (reverse-row/acc my-row acc) returns 'my-row', but reversed
     ;; reverse-row/acc: (listof Num) (listof Num) -> (listof Num)
     (define (reverse-row/acc my-row acc)
       (cond
         [(empty? my-row) acc]
         [else
          (reverse-row/acc (rest my-row) (cons (first my-row) acc))]))]
    (cond
      [(empty? my-table) empty]
      [else (cons (reverse-row (first my-table))
                  (mirror (rest my-table)))])))

;; Tests
(check-expect (mirror '((1 2 3)
                        (4 4 4)
                        (-3 0 3)))
              '((3 2 1)
                (4 4 4)
                (3 0 -3)))
(check-expect (mirror '((1 1 1)
                        (1 1 1)))
              '((1 1 1)
                (1 1 1)))


;; (add3 x) adds 3 to the given number 'x'
;; add3: Num -> Num
;; Examples
(check-expect (add3 6) 9)
(check-expect (add3 -2.4) 0.6)

(define (add3 x) (+ x 3))


;; (mod3 x) returns the remainder of the given number 'x' divided by 3
;; mod3: Int -> Int
;; Examples
(check-expect (mod3 6) 0)
(check-expect (mod3 -4) -1)

(define (mod3 x) (remainder x 3))


;; (apply-to-table fn my-table) applies a given function 'fn' to the given
;;    table 'my-table'
;; apply-to-table: (anyof (Num -> Num)
;;                        (Num -> Int)
;;                        (Num -> Nat))) Table -> Table
;; Examples
(check-expect (apply-to-table ceiling '((0.2 0.8 1.2)
                                        (-0.2 -0.9 -1.2)))
              '((1 1 2)
                (0 0 -1)))
(check-expect (apply-to-table add1 '(() () ()))
              '(() () ()))
(check-expect (apply-to-table add3 (list empty))
              (list empty))

(define (apply-to-table fn my-table)
  (cond
    [(empty? my-table) empty]
    [else
     (cons (apply-to-row fn (first my-table))
           (apply-to-table fn (rest my-table)))]))


;; (apply-to-row fn my-row) applies a given function 'fn' to the given
;;     row 'my-row'
;; apply-to-row: (anyof (Num -> Num)
;;                      (Num -> Int)
;;                      (Num -> Nat))) (listof Num) -> (listof Num)
;; Examples
(check-expect (apply-to-row add1 '(-2 -1 0 1 2)) '(-1 0 1 2 3))
(check-expect (apply-to-row add1 empty) empty)
(check-expect (apply-to-row add3 '(1)) '(4))

(define (apply-to-row fn my-row)
  (cond
    [(empty? my-row) empty]
    [else
     (cons (fn (first my-row))
           (apply-to-row fn (rest my-row)))]))


;; (element-apply-many lofunctions my-table) produces a list of tables each
;;    with the respective function from 'lofunctions' applied to 'my-table'
;; element-apply-many: (listof (anyof (Num -> Num)
;;                                    (Num -> Int)
;;                                    (Num -> Nat))) Table -> (listof Table)
;; requires: lofunctions must not be empty
;; Examples
(check-expect (element-apply-many (list add3 add1) '((1 2 3)
                                                     (3 4 5)
                                                     (-2 -4 0)))
              '(((4 5 6)
                 (6 7 8)
                 (1 -1 3))
                
                ((2 3 4)
                 (4 5 6)
                 (-1 -3 1))))
(check-expect (element-apply-many (list add1) '(() () ()))
              '((() () ())))
(check-expect (element-apply-many (list add1) (list empty))
              (list (list empty)))

(define (element-apply-many lofunctions my-table)
  (cond
    [(empty? lofunctions) empty]
    [else
     (cons (apply-to-table (first lofunctions) my-table)
           (element-apply-many (rest lofunctions) my-table))]))

;; Tests
(check-expect (element-apply-many (list floor) '((0.2 0.8 1.2)
                                                 (-0.2 -0.9 -1.2)))
              '(((0 0 1)
                 (-1 -1 -2))))
(check-expect (element-apply-many (list add1 add3) (list empty))
              (list (list empty)
                    (list empty)))
(check-expect (element-apply-many (list mod3 add3 mod3) '((9 7 3)
                                                          (10 3 4)
                                                          (-5 -7 0)))
              '(((0 1 0)
                 (1 0 1)
                 (-2 -1 0))
                
                ((12 10 6)
                 (13 6 7)
                 (-2 -4 3))
                
                ((0 1 0)
                 (1 0 1)
                 (-2 -1 0))))


;; (apply-function fn arg) produces the result of 'fn' with the
;;    given parameter 'arg'
;; apply-function: (Num -> Num) Num -> Num
;; Examples
(check-expect (apply-function add3 2) 5)
(check-expect (apply-function add1 -3.2) -2.2)

(define (apply-function fn arg)
  (fn arg))


;; (find-smallest my-table) returns the lowest value from 'my-table'
;; find-smallest: Table -> Num
;; requires: my-table must have >= 1 row and >= 1 column
;; Examples
(check-expect (find-smallest '((7 4.5 3.2)
                               (-3 3 13)))
              -3)
(check-expect (find-smallest '((4))) 4)
(check-expect (find-smallest '((3) (2))) 2)

(define (find-smallest my-table)
  (find-smallest/acc my-table (first (first my-table))))


;; (find-smallest/acc my-table current-min) returns the lowest value
;;     from 'my-table' 
;; find-smallest/acc: Table Num -> Num
;; requires: my-table must have >= 1 row and >= 1 column
;; Examples
(check-expect (find-smallest/acc '((7 4.5 3.2)
                               (-3 3 13)) 7)
              -3)
(check-expect (find-smallest/acc '((4)) 4) 4)
(check-expect (find-smallest/acc '((3) (2)) 3) 2)

(define (find-smallest/acc my-table current-min)
  (cond
    [(empty? my-table) current-min]
    [else
     (min (find-smallest-row (first my-table) current-min)
          (find-smallest/acc (rest my-table) current-min))]))


;; (find-smallest-row my-row current-min) returns the lowest value from 'my-row'
;; find-smallest-row: (listof Num) Num -> Num
;; requires: my-row must not be empty
;; Examples
(check-expect (find-smallest-row '(2 3 4 1) 2) 1)
(check-expect (find-smallest-row '(0 0 0) 0) 0)

(define (find-smallest-row my-row current-min)
  (cond
    [(empty? my-row) current-min]
    [(< (first my-row) current-min)
     (find-smallest-row (rest my-row) (first my-row))]
    [else
     (find-smallest-row (rest my-row) current-min)]))
  

;; (scale-smallest my-table offset) produces a function that 
;;    consumes a number, multiplies it by the smallest number 
;;    in 'my-table', and adds the 'offset'
;; scale-smallest: Table Num -> (Num -> Num)
;; requires: my-table must have >= 1 row and >= 1 column
;; Examples
(check-expect (apply-function (scale-smallest '((7 4.5 3.2)
                                                (-3 3 13)) 2.4)
                              7)
              -18.6)
(check-expect (apply-function (scale-smallest '((0 2 4)) 4)
                              4)
              4)

(define (scale-smallest my-table offset)
  (local
    [(define (mult-then-add n)
       (+ (* n (find-smallest my-table)) offset))]
    mult-then-add))

;; Tests
(check-expect (apply-function (scale-smallest '((3)) 1)
                              2)
              7)
(check-expect (apply-function (scale-smallest '((3) (2) (1) (-3)) 2)
                              5)
              -13)
(check-expect (apply-function (scale-smallest '((7 4.5 3.2)
                                                (-3 3 13)) 2.4)
                              -2.7)
              10.5)
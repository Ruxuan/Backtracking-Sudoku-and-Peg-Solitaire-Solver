;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The following line is REQUIRED (do not remove)
(require "a10lib.rkt")


;; Place your Personal Identification here

;;
;;*********************************************************
;; CS135 - 004 Fall 2014
;; Ruxuan Li (20556099)
;; A10, sudoku.rkt
;;*********************************************************
;;

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A SudokuDigit is one of:
;; * '?
;; * 1 <= Nat <= 9

;; A Puzzle is a (listof (listof SudokuDigit))
;; requires: the list and all sublists have a length of 9

;; A Solution is a Puzzle
;; requires: none of the SudokuDigits are '?
;;           the puzzle satisfies the number placement 
;;             rules of sudoku

;; A Coordn corresponds to a position/coordinate on a sudoku board and is a
;; * (list Nat Nat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some sample sudoku puzzles

;; From the basic test shown in the assignment:
(define veryeasy
  '((? 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

;; the above puzzle with more blanks:
(define easy
  '((? 4 5 8 ? 3 7 1 ?)
    (8 1 ? ? ? ? ? 2 4)
    (7 ? 9 ? ? ? 5 ? 8)
    (? ? ? 9 ? 7 ? ? ?)
    (? ? ? ? 6 ? ? ? ?)
    (? ? ? 4 ? 2 ? ? ?)
    (6 ? 4 ? ? ? 3 ? 5)
    (3 2 ? ? ? ? ? 8 7)
    (? 5 7 3 ? 8 2 6 ?)))

;; the puzzle listed on wikipedia
(define wikipedia '((5 3 ? ? 7 ? ? ? ?)
                    (6 ? ? 1 9 5 ? ? ?)
                    (? 9 8 ? ? ? ? 6 ?)
                    (8 ? ? ? 6 ? ? ? 3)
                    (4 ? ? 8 ? 3 ? ? 1)
                    (7 ? ? ? 2 ? ? ? 6)
                    (? 6 ? ? ? ? 2 8 ?)
                    (? ? ? 4 1 9 ? ? 5)
                    (? ? ? ? 8 ? ? 7 9)))

;; A blank puzzle template for you to use:
(define blank '((? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HAVE FUN, good luck with your final exams, and have a Merry Christmas!
;; -- with love, the CS 135 team

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Section is one of the following:
(define section-1 '((1 1) (1 2) (1 3)
                    (2 1) (2 2) (2 3)
                    (3 1) (3 2) (3 3)))
(define section-2 '((1 4) (1 5) (1 6)
                    (2 4) (2 5) (2 6)
                    (3 4) (3 5) (3 6)))
(define section-3 '((1 7) (1 8) (1 9)
                    (2 7) (2 8) (2 9)
                    (3 7) (3 8) (3 9)))

(define section-4 '((4 1) (4 2) (4 3)
                    (5 1) (5 2) (5 3)
                    (6 1) (6 2) (6 3)))
(define section-5 '((4 4) (4 5) (4 6)
                    (5 4) (5 5) (5 6)
                    (6 4) (6 5) (6 6)))
(define section-6 '((4 7) (4 8) (4 9)
                    (5 7) (5 8) (5 9)
                    (6 7) (6 8) (6 9)))

(define section-7 '((7 1) (7 2) (7 3)
                    (8 1) (8 2) (8 3)
                    (9 1) (9 2) (9 3)))
(define section-8 '((7 4) (7 5) (7 6)
                    (8 4) (8 5) (8 6)
                    (9 4) (9 5) (9 6)))
(define section-9 '((7 7) (7 8) (7 9)
                    (8 7) (8 8) (8 9)
                    (9 7) (9 8) (9 9)))

;; Constants
(define coordinates (build-list 9 (lambda (x) (build-list 9 (lambda (y) (list (add1 x) 
                                                                              (add1 y)))))))

(define solved? (lambda (x) (empty? (get-blank x))))

;;******************************************************************************
;; (get-spot board pair) gets the element on a puzzle's (board)
;;     Coordn (pair).
;; get-spot: Puzzle Coordn -> SudokuDigit
;; Examples:
(check-expect (get-spot veryeasy '(0 1)) 4)
(check-expect (get-spot easy '(1 3)) '?)

(define (get-spot board pair)
  (list-ref (list-ref board (first pair)) (second pair)))

;; Tests:
(check-expect (get-spot easy '(8 7)) 6)
(check-expect (get-spot wikipedia '(3 2)) '?)
(check-expect (get-spot wikipedia '(7 4)) 1)
(check-expect (get-spot wikipedia '(5 0)) 7)
;;******************************************************************************
;; (get-blank board) returns a list of Coordn that
;;     has the symbol '? as its value.
;; get-blank: Board -> (listof Coordn)
;; Examples:
(check-expect (get-blank veryeasy) '((1 1)))
(check-expect (get-blank easy) '((1 1) (1 5) (1 9) (2 3) (2 4) (2 5) (2 6)
                                 (2 7) (3 2) (3 4) (3 5) (3 6) (3 8) (4 1)
                                 (4 2) (4 3) (4 5) (4 7) (4 8) (4 9) (5 1)
                                 (5 2) (5 3) (5 4) (5 6) (5 7) (5 8) (5 9)
                                 (6 1) (6 2) (6 3) (6 5) (6 7) (6 8) (6 9)
                                 (7 2) (7 4) (7 5) (7 6) (7 8) (8 3) (8 4)
                                 (8 5) (8 6) (8 7) (9 1) (9 5) (9 9)))

(define (get-blank board)
  (foldr append empty (map (lambda (x) 
                             (filter (lambda (y) 
                                       (equal? '? (get-spot board (list (sub1 (first y))
                                                                        (sub1 (second y)))))) x)) coordinates)))

;; Tests:
(check-expect (get-blank wikipedia) (list (list 1 3) (list 1 4) (list 1 6) (list 1 7) (list 1 8)
                                          (list 1 9) (list 2 2) (list 2 3) (list 2 7) (list 2 8)
                                          (list 2 9) (list 3 1) (list 3 4) (list 3 5) (list 3 6)
                                          (list 3 7) (list 3 9) (list 4 2) (list 4 3) (list 4 4)
                                          (list 4 6) (list 4 7) (list 4 8) (list 5 2) (list 5 3)
                                          (list 5 5) (list 5 7) (list 5 8) (list 6 2) (list 6 3)
                                          (list 6 4) (list 6 6) (list 6 7) (list 6 8) (list 7 1)
                                          (list 7 3) (list 7 4) (list 7 5) (list 7 6) (list 7 9)
                                          (list 8 1) (list 8 2) (list 8 3) (list 8 7) (list 8 8)
                                          (list 9 1) (list 9 2) (list 9 3) (list 9 4) (list 9 6)
                                          (list 9 7)))

(check-expect (get-blank blank) (list (list 1 1) (list 1 2) (list 1 3) (list 1 4) (list 1 5)
                                      (list 1 6) (list 1 7) (list 1 8) (list 1 9) (list 2 1)
                                      (list 2 2) (list 2 3) (list 2 4) (list 2 5) (list 2 6)
                                      (list 2 7) (list 2 8) (list 2 9) (list 3 1) (list 3 2)
                                      (list 3 3) (list 3 4) (list 3 5) (list 3 6) (list 3 7)
                                      (list 3 8) (list 3 9) (list 4 1) (list 4 2) (list 4 3)
                                      (list 4 4) (list 4 5) (list 4 6) (list 4 7) (list 4 8)
                                      (list 4 9) (list 5 1) (list 5 2) (list 5 3) (list 5 4)
                                      (list 5 5) (list 5 6) (list 5 7) (list 5 8) (list 5 9)
                                      (list 6 1) (list 6 2) (list 6 3) (list 6 4) (list 6 5)
                                      (list 6 6) (list 6 7) (list 6 8) (list 6 9) (list 7 1)
                                      (list 7 2) (list 7 3) (list 7 4) (list 7 5) (list 7 6)
                                      (list 7 7) (list 7 8) (list 7 9) (list 8 1) (list 8 2)
                                      (list 8 3) (list 8 4) (list 8 5) (list 8 6) (list 8 7)
                                      (list 8 8) (list 8 9) (list 9 1) (list 9 2) (list 9 3)
                                      (list 9 4) (list 9 5) (list 9 6) (list 9 7) (list 9 8)
                                      (list 9 9)))
(check-expect (get-blank '((2 4 5 8 9 3 7 1 6)
                           (8 1 3 5 7 6 9 2 4)
                           (7 6 9 2 1 4 5 3 8)
                           (5 3 6 9 8 7 1 4 2)
                           (4 9 2 1 6 5 8 7 3)
                           (1 7 8 4 3 2 6 5 9)
                           (6 8 4 7 2 1 3 9 5)
                           (3 2 1 6 5 9 4 8 7)
                           (9 5 7 3 4 8 2 6 1)))
              '())
;;******************************************************************************
;; (dedup lst) consumes a list of numbers (lst) and produces
;;     a new list with only one occurrence of each element in
;;     the original list.
;; dedup: (listof Num) -> (listof Num)
;; Examples:
(check-expect (dedup '(1 2 3 4 5 5 6 6 7)) '(1 2 3 4 5 6 7))
(check-expect (dedup '(11 2 2 33 4 4 55 6 6)) '(11 2 33 4 55 6))

(define (dedup lst)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y)))
         empty lst))

;; Tests:
(check-expect (dedup '(12 35 48 67 8 9 11 325)) '(12 35 48 67 8 9 11 325))
(check-expect (dedup '(0 0 0 0 0 0)) '(0))
(check-expect (dedup '(1 1 1 1 1)) '(1))
(check-expect (dedup empty) empty)
(check-expect (dedup '(9 8 7 6 5 4 3 1)) '(9 8 7 6 5 4 3 1))
;;******************************************************************************
;; (section puzzle x) finds all the numbers in the puzzle that
;;     are also in the given Coordn's (x) section.
;; section: Puzzle Coordn -> (listof Nat)
;; Examples:
(check-expect (section veryeasy '(5 5)) '(9 8 7 1 6 5 4 3 2))
(check-expect (section veryeasy '(4 4)) '(9 8 7 1 6 5 4 3 2))
(check-expect (section veryeasy '(1 1)) '(4 5 8 1 3 7 6 9))             
              
(define (section puzzle x)
            (local [;; (find-section x) determines which section is the
                    ;;     given Coordn (x) from.
                    ;; find-section: Coord -> Section
                    (define (find-section x)
                      (cond [(< (first x) 4)
                             (cond [(< (second x) 4) section-1]
                                   [(< (second x) 7) section-2]
                                   [else section-3])]                            
                            [(< (first x) 7)
                             (cond [(< (second x) 4) section-4]
                                   [(< (second x) 7) section-5]
                                   [else section-6])]    
                            [else 
                             (cond [(< (second x) 4) section-7]
                                   [(< (second x) 7) section-8]
                                   [else section-9])]))]    
              (filter (lambda (l) (not (equal? '? l))) 
                      (map (lambda (p) (get-spot puzzle (list (sub1 (first p))
                                                              (sub1 (second p))))) (find-section x)))))

;; Tests:
(check-expect (section easy '(3 4)) '(8 3))
(check-expect (section easy '(8 4)) '(3 8))
(check-expect (section easy '(9 9)) '(3 5 8 7 2 6))
(check-expect (section easy '(7 2)) '(6 4 3 2 5 7))
(check-expect (section easy '(1 9)) '(7 1 2 4 5 8))
(check-expect (section wikipedia '(4 5)) '(6 8 3 2))
(check-expect (section wikipedia '(7 8)) '(2 8 5 7 9))
(check-expect (section wikipedia '(6 1)) '(8 4 7))
(check-expect (section blank '(5 6)) '())
(check-expect (section blank '(8 9)) '())
;;******************************************************************************
;; (invalid-numbers puzzle coordinate) finds all numbers that a Coordn (coordinate)
;;     cannot be based on the rules of sudoku. The function takes a puzzle and a
;;     Coordn (coordinate).
;; invalid-numbers: Puzzle Coordn -> (listof Nat)
;; Examples:
(check-expect (invalid-numbers veryeasy '(1 1)) '(1 3 4 5 6 7 8 9))
(check-expect (invalid-numbers veryeasy '(3 2)) '(1 2 3 4 5 6 7 8 9))

(define (invalid-numbers puzzle coordinate)
  (local [;; (row puzzle x) finds all numbers in a given puzzle
          ;;     that a given Coordn (x) cannot be based on the
          ;;     Coordn's row. Valid numbers are based on 
          ;;     sudoku rules.
          ;; row: Puzzle Coordn -> (listof Nat)
          (define (row puzzle x)
            (filter (lambda (x) (not (equal? x '?))) (list-ref puzzle (sub1 x))))
          
          ;; (column puzzle x) finds all the numbers in a given puzzle
          ;;     that a given Coordn (x) cannot be based on the Coordn's
          ;;     column. Valid numbers are again based on sudoku rules.
          ;; column: Puzzle Coordn -> (listof Nat)
          (define (column puzzle x)
            (local [;; (return-column puzzle x) puts all the values of a column
                    ;;     into a list. The main function then filters out the
                    ;;     '? in this list.
                    ;; return-column: Puzzle Coordn -> (listof SudokuDigit)
                    (define (return-column puzzle x)
                      (cond [(empty? puzzle) empty]
                            [else 
                             (cons (list-ref (first puzzle) x)
                                   (return-column (rest puzzle) x))]))]
              (filter (lambda (s) (not (equal? s '?))) (return-column puzzle (sub1 x)))))]
    (sort (dedup (append (row puzzle (first coordinate))
                         (column puzzle (second coordinate))
                         (section puzzle coordinate))) <)))

;; Tests:
(check-expect (invalid-numbers easy '(5 4)) '(2 3 4 6 7 8 9))
(check-expect (invalid-numbers easy '(9 9)) '(2 3 4 5 6 7 8))
(check-expect (invalid-numbers easy '(7 1)) '(2 3 4 5 6 7 8))
(check-expect (invalid-numbers easy '(2 9)) '(1 2 4 5 7 8))
(check-expect (invalid-numbers wikipedia '(3 3)) '(3 5 6 8 9))
(check-expect (invalid-numbers wikipedia '(4 8)) '(1 3 6 7 8))
(check-expect (invalid-numbers wikipedia '(9 8)) '(2 5 6 7 8 9))
;;******************************************************************************
;; (change-spot puzzle pair new) builds the given Puzzle with the element in
;;     the Coordn (pair) switched with the given new element (new).
;; change-spot: Puzzle Coordn Nat -> Puzzle
;; Examples:
(check-expect (change-spot veryeasy '(1 1) 5)
              '((5 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
(check-expect (change-spot easy '(9 9) 4)
              '((? 4 5 8 ? 3 7 1 ?)
                (8 1 ? ? ? ? ? 2 4)
                (7 ? 9 ? ? ? 5 ? 8)
                (? ? ? 9 ? 7 ? ? ?)
                (? ? ? ? 6 ? ? ? ?)
                (? ? ? 4 ? 2 ? ? ?)
                (6 ? 4 ? ? ? 3 ? 5)
                (3 2 ? ? ? ? ? 8 7)
                (? 5 7 3 ? 8 2 6 4)))              

(define (change-spot puzzle pair new)
  (build-list 9 
              (lambda (r) (build-list 9 
                                      (lambda (c) (cond [(equal? (list (sub1 (first pair))
                                                                       (sub1 (second pair)))
                                                                 (list r c))
                                                         new]
                                                        [else (get-spot puzzle (list r c))]))))))

;; Tests:
(check-expect (change-spot easy '(3 8) 9)
              '((? 4 5 8 ? 3 7 1 ?)
                (8 1 ? ? ? ? ? 2 4)
                (7 ? 9 ? ? ? 5 9 8)
                (? ? ? 9 ? 7 ? ? ?)
                (? ? ? ? 6 ? ? ? ?)
                (? ? ? 4 ? 2 ? ? ?)
                (6 ? 4 ? ? ? 3 ? 5)
                (3 2 ? ? ? ? ? 8 7)
                (? 5 7 3 ? 8 2 6 ?)))
(check-expect (change-spot easy '(4 2) 8)
              '((? 4 5 8 ? 3 7 1 ?)
                (8 1 ? ? ? ? ? 2 4)
                (7 ? 9 ? ? ? 5 ? 8)
                (? 8 ? 9 ? 7 ? ? ?)
                (? ? ? ? 6 ? ? ? ?)
                (? ? ? 4 ? 2 ? ? ?)
                (6 ? 4 ? ? ? 3 ? 5)
                (3 2 ? ? ? ? ? 8 7)
                (? 5 7 3 ? 8 2 6 ?)))
(check-expect (change-spot easy '(9 5) 4)
              '((? 4 5 8 ? 3 7 1 ?)
                (8 1 ? ? ? ? ? 2 4)
                (7 ? 9 ? ? ? 5 ? 8)
                (? ? ? 9 ? 7 ? ? ?)
                (? ? ? ? 6 ? ? ? ?)
                (? ? ? 4 ? 2 ? ? ?)
                (6 ? 4 ? ? ? 3 ? 5)
                (3 2 ? ? ? ? ? 8 7)
                (? 5 7 3 4 8 2 6 ?)))
(check-expect (change-spot wikipedia '(4 4) 7)
              '((5 3 ? ? 7 ? ? ? ?)
                (6 ? ? 1 9 5 ? ? ?)
                (? 9 8 ? ? ? ? 6 ?)
                (8 ? ? 7 6 ? ? ? 3)
                (4 ? ? 8 ? 3 ? ? 1)
                (7 ? ? ? 2 ? ? ? 6)
                (? 6 ? ? ? ? 2 8 ?)
                (? ? ? 4 1 9 ? ? 5)
                (? ? ? ? 8 ? ? 7 9)))
(check-expect (change-spot wikipedia '(6 4) 5)
              '((5 3 ? ? 7 ? ? ? ?)
                (6 ? ? 1 9 5 ? ? ?)
                (? 9 8 ? ? ? ? 6 ?)
                (8 ? ? ? 6 ? ? ? 3)
                (4 ? ? 8 ? 3 ? ? 1)
                (7 ? ? 5 2 ? ? ? 6)
                (? 6 ? ? ? ? 2 8 ?)
                (? ? ? 4 1 9 ? ? 5)
                (? ? ? ? 8 ? ? 7 9)))
;;******************************************************************************
;; (neighbours puzzle) consumes a Puzzle and finds a list of
;;     all the possible next moves.
;; neighbours: Puzzle -> (listof Puzzle)
;; Examples:
(check-expect (neighbours '((2 4 5 8 ? 3 7 1 ?)
                            (8 1 ? ? ? ? ? 2 4)
                            (7 ? 9 ? ? ? 5 ? 8)
                            (? ? ? 9 ? 7 ? ? ?)
                            (? ? ? ? 6 ? ? ? ?)
                            (? ? ? 4 ? 2 ? ? ?)
                            (6 ? 4 ? ? ? 3 ? 5)
                            (3 2 ? ? ? ? ? 8 7)
                            (? 5 7 3 ? 8 2 6 ?)))
              '(((2 4 5 8 9 3 7 1 ?)
                 (8 1 ? ? ? ? ? 2 4)
                 (7 ? 9 ? ? ? 5 ? 8)
                 (? ? ? 9 ? 7 ? ? ?)
                 (? ? ? ? 6 ? ? ? ?)
                 (? ? ? 4 ? 2 ? ? ?)
                 (6 ? 4 ? ? ? 3 ? 5)
                 (3 2 ? ? ? ? ? 8 7)
                 (? 5 7 3 ? 8 2 6 ?))))
(check-expect (neighbours '((2 4 5 8 9 3 7 1 ?)
                            (8 1 ? ? ? ? ? 2 4)
                            (7 ? 9 ? ? ? 5 ? 8)
                            (? ? ? 9 ? 7 ? ? ?)
                            (? ? ? ? 6 ? ? ? ?)
                            (? ? ? 4 ? 2 ? ? ?)
                            (6 ? 4 ? ? ? 3 ? 5)
                            (3 2 ? ? ? ? ? 8 7)
                            (? 5 7 3 ? 8 2 6 ?)))
              '(((2 4 5 8 9 3 7 1 6)
                 (8 1 ? ? ? ? ? 2 4)
                 (7 ? 9 ? ? ? 5 ? 8)
                 (? ? ? 9 ? 7 ? ? ?)
                 (? ? ? ? 6 ? ? ? ?)
                 (? ? ? 4 ? 2 ? ? ?)
                 (6 ? 4 ? ? ? 3 ? 5)
                 (3 2 ? ? ? ? ? 8 7)
                 (? 5 7 3 ? 8 2 6 ?))))

(define (neighbours puzzle)
  (local [(define blank-pos (first (get-blank puzzle)))
          (define valid-neighbours
            (filter (lambda (x) (not (member? x (invalid-numbers puzzle blank-pos)))) 
                    (build-list 9 add1)))]
    (map (lambda (s) (change-spot puzzle blank-pos s)) valid-neighbours)))


(check-expect (neighbours veryeasy)
              '(((2 4 5 8 9 3 7 1 6)
                 (8 1 3 5 7 6 9 2 4)
                 (7 6 9 2 1 4 5 3 8)
                 (5 3 6 9 8 7 1 4 2)
                 (4 9 2 1 6 5 8 7 3)
                 (1 7 8 4 3 2 6 5 9)
                 (6 8 4 7 2 1 3 9 5)
                 (3 2 1 6 5 9 4 8 7)
                 (9 5 7 3 4 8 2 6 1))))
(check-expect (neighbours easy)
              '(((2 4 5 8 ? 3 7 1 ?)
                 (8 1 ? ? ? ? ? 2 4)
                 (7 ? 9 ? ? ? 5 ? 8)
                 (? ? ? 9 ? 7 ? ? ?)
                 (? ? ? ? 6 ? ? ? ?)
                 (? ? ? 4 ? 2 ? ? ?)
                 (6 ? 4 ? ? ? 3 ? 5)
                 (3 2 ? ? ? ? ? 8 7)
                 (? 5 7 3 ? 8 2 6 ?))))
(check-expect (neighbours wikipedia)
              '(((5 3 1 ? 7 ? ? ? ?)
                 (6 ? ? 1 9 5 ? ? ?)
                 (? 9 8 ? ? ? ? 6 ?)
                 (8 ? ? ? 6 ? ? ? 3)
                 (4 ? ? 8 ? 3 ? ? 1)
                 (7 ? ? ? 2 ? ? ? 6)
                 (? 6 ? ? ? ? 2 8 ?)
                 (? ? ? 4 1 9 ? ? 5)
                 (? ? ? ? 8 ? ? 7 9))
                ((5 3 2 ? 7 ? ? ? ?)
                 (6 ? ? 1 9 5 ? ? ?)
                 (? 9 8 ? ? ? ? 6 ?)
                 (8 ? ? ? 6 ? ? ? 3)
                 (4 ? ? 8 ? 3 ? ? 1)
                 (7 ? ? ? 2 ? ? ? 6)
                 (? 6 ? ? ? ? 2 8 ?)
                 (? ? ? 4 1 9 ? ? 5)
                 (? ? ? ? 8 ? ? 7 9))
                ((5 3 4 ? 7 ? ? ? ?)
                 (6 ? ? 1 9 5 ? ? ?)
                 (? 9 8 ? ? ? ? 6 ?)
                 (8 ? ? ? 6 ? ? ? 3)
                 (4 ? ? 8 ? 3 ? ? 1)
                 (7 ? ? ? 2 ? ? ? 6)
                 (? 6 ? ? ? ? 2 8 ?)
                 (? ? ? 4 1 9 ? ? 5)
                 (? ? ? ? 8 ? ? 7 9))))
(check-expect (neighbours blank)
              '(((1 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((2 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((3 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((4 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((5 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((6 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((7 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((8 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))
                ((9 ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?)
                 (? ? ? ? ? ? ? ? ?))))
;;********************************************************************
;;********************************************************************
;; (sudoku puzzle) is the solver function. Sudoku solves
;;     a given Puzzle fully and produces the final result.
;; sudoku: Puzzle -> Puzzle
;; Examples:
(check-expect (sudoku veryeasy)
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))

(define (sudoku puzzle)
  (find-final puzzle neighbours solved?))

(check-expect (sudoku easy)
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
(check-expect (sudoku wikipedia)
              '((5 3 4 6 7 8 9 1 2)
                (6 7 2 1 9 5 3 4 8)
                (1 9 8 3 4 2 5 6 7)
                (8 5 9 7 6 1 4 2 3)
                (4 2 6 8 5 3 7 9 1)
                (7 1 3 9 2 4 8 5 6)
                (9 6 1 5 3 7 2 8 4)
                (2 8 7 4 1 9 6 3 5)
                (3 4 5 2 8 6 1 7 9)))
;;********************************************************************
;;********************************************************************
(define very-difficult-sudoku '((? ? 8 ? ? 3 ? ? 1)
                                (? 6 ? ? 4 ? ? 9 ?)
                                (3 ? ? 9 ? ? 7 ? ?)
                                (8 ? ? 2 ? ? 1 ? ?)
                                (? 2 ? ? 7 ? ? 5 ?)
                                (? ? 5 ? ? 9 ? ? 6)
                                (? ? 4 ? ? 6 ? ? 7)
                                (? 5 ? ? 9 ? ? 1 ?)
                                (7 ? ? 3 ? ? 4 ? ?)))
(check-expect (sudoku very-difficult-sudoku)
              (list
               (list 5 9 8 7 2 3 6 4 1)
               (list 2 6 7 1 4 8 5 9 3)
               (list 3 4 1 9 6 5 7 8 2)
               (list 8 7 6 2 5 4 1 3 9)
               (list 9 2 3 6 7 1 8 5 4)
               (list 4 1 5 8 3 9 2 7 6)
               (list 1 3 4 5 8 6 9 2 7)
               (list 6 5 2 4 9 7 3 1 8)
               (list 7 8 9 3 1 2 4 6 5)))
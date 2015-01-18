;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solitaire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The following line is REQUIRED (do not remove)
(require "a10lib.rkt")


;; Place your Personal Identification here
;;
;;*********************************************************
;; CS135 - 004 Fall 2014
;; Ruxuan Li (20556099)
;; A10, solitaire.rkt
;;*********************************************************
;;

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg

(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the sample board from the assignment

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

(define cross/pyramid-16 (list 24 33 34 35 42 43 44 45 46
                               51 52 53 54 55 56 57))

(define five-x-five (list 11 12 13 14 15
                          21 22 23 24 25
                          31 32 33 34 35
                          41 42    44 45
                          51 52 53 54 55))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;******************************************************************************
;; (build-board dim) takes a Dimension (dim) and produces
;;     a board in the format of (listof (listof Peg)) where
;;     the list corresponds to a list of rows and each row
;;     contains a list of Peg positions in that row.
;; build-board: Nat -> (listof (listof Peg))
;; Examples:
(check-expect (build-board 3) '((11 12 13)
                                (21 22 23)
                                (31 32 33)))
(check-expect (build-board 1) '((11)))

(define (build-board dim)
  (build-list dim (lambda (x)
                    (build-list dim (lambda (y) 
                                      (+ (+ 1 y) 
                                         (+ (* x 10) 10)))))))

;; Tests:
(check-expect (build-board 2) '((11 12)
                                (21 22)))
(check-expect (build-board 0) '())
(check-expect (build-board 5) '((11 12 13 14 15)
                                (21 22 23 24 25)
                                (31 32 33 34 35)
                                (41 42 43 44 45)
                                (51 52 53 54 55)))
;;******************************************************************************
;; (state->los board state) takes a Board and a State and produces
;;     a visualization of the current Board. The visualization will
;;     be in the form of a (listof Str). One string per row will be
;;     used to represent the positions and each character in the string
;;     will correspon to a Peg position, with a #\space representing an
;;     invalid position. #\O (capital O not zero) will represent a Peg
;;     while #\. will represent an unoccupied spot.
;; state->los: Board State -> (listof Str)
;; Examples:
(check-expect (state->los '(4 (41 42 43 44)) '(22 23))
              '("...." ".OO." "...." "    "))
(check-expect (state->los '(1 ()) '(11)) '("O"))
              

(define (state->los board state)
  (map list->string (map (lambda (x) (map (lambda (y) (cond[(member? y state) #\O]
                                                           [(member? y (second board)) #\space]
                                                           [else #\.])) x))
                         (build-board (first board)))))

;; Tests:
(check-expect (state->los '(9 (11 12 13 21 22 23 31 32 33
                                  17 18 19 27 28 29 37 38 39
                                  71 72 73 81 82 83 91 92 93
                                  77 78 79 87 88 89 97 98 99))
                          '(14 15 16 24 25 26 34 35 36
                               41 42 43 44 45 46 47 48 49
                               51 52 53 54    56 57 58 59
                               61 62 63 64 65 66 67 68 69
                               74 75 76 84 85 86 94 95 96))
              '("   OOO   "
                "   OOO   "
                "   OOO   "
                "OOOOOOOOO"
                "OOOO.OOOO"
                "OOOOOOOOO"
                "   OOO   "
                "   OOO   "
                "   OOO   "))
(check-expect (state->los (list 9 '(11 12 13 14 15 16 17 18 19
                                       21 31 41 51 61 71 81 91
                                       29 39 49 59 69 79 89 99
                                       92 93 94 95 96 97 98))
                          '(55 56 58))
              '("         "
                " ....... "
                " ....... "
                " ....... "
                " ...OO.O "
                " ....... "
                " ....... "
                " ....... "
                "         "))
(check-expect (state->los cross cross/init)
              '("  OOO  "
                "  OOO  "
                "OOOOOOO"
                "OOO.OOO"
                "OOOOOOO"
                "  OOO  "
                "  OOO  "))
;;******************************************************************************
;; (make-solved? sol) consumes a Solution (sol) and generates
;;     a new predicate function that consumes a State and produces
;;     true if the State is a solution and false otherwise.
;; make-solved?: Solution -> ((listof Any) -> Bool)
;; Examples :
(check-expect ((make-solved? 'any) (list 1 2)) false)
(check-expect ((make-solved? 'any) (list 10)) true)
(check-expect ((make-solved? 'any) (list 88)) true)
(check-expect ((make-solved? 22) (list 22)) true)

(define (make-solved? sol)
  (cond [(number? sol)
         (lambda (x) (and (= (length x) 1)
                          (= sol (first x))))]
        [else
         (lambda (x) (= (length x) 1))]))
   
;; Tests:
(define my-solved-2 (make-solved? 44))
(check-expect (my-solved-2 (list 44)) true)
(check-expect (my-solved-2 (list 44 45)) false)
(check-expect (my-solved-2 (list 68)) false)

(define my-solved-1 (make-solved? 'any))
(check-expect (my-solved-1 (list 44 45)) false)
(check-expect (my-solved-1 (list 44)) true)
(check-expect (my-solved-1 (list 99 11)) false)
;;******************************************************************************
;; (neighbours board state) consumes a Board (board) and a State
;;     (state) and produces a list of possible next moves.
;;     The list of possible next moves will be in the form of
;;     a (listof State).
;; neighbours: Board State -> (listof State)
;; Examples:
(check-expect (neighbours '(4 ()) '(21 22 24))
              '((23 24)))
(check-expect (neighbours '(3 ()) '(11 12 21))
              '((13 21) (12 31)))
(check-expect (neighbours '(5 (21 22 23 24)) '(31 33 41 43))
              '((33 43 51) (31 41 53)))
(check-expect (neighbours '(4 ()) '(12 13 21 31))
              '((14 21 31) (11 21 31) (12 13 41) (11 12 13)))

(define (neighbours board state)
  (foldr (lambda (x y) (append (possible-moves board state x '()) y)) empty state))

;; Tests:
(check-expect (neighbours cross cross/init)
              '((13 14 15 23 25 31 32 33 35 36 37 41 42
                 43 44 45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 44
                 45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                 44 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43 44
                 45 46 47 51 52 53 55 56 57 63 65 73 74 75)))

(check-expect (neighbours '(4 ()) '(11 14 22 24 33))
              '((11 22 33 34)))
(check-expect (neighbours '(3 (12 31)) '(11 21 22 32))
              '((11 23 32)))
(check-expect (neighbours '(4 (22 23 42 43)) '(11 12 13 32 33 44))
              '((11 14 32 33 44) (11 12 13 34 44) (11 12 13 31 44)))
(check-expect (neighbours cross cross/init)
              '((13 14 15 23 25 31 32 33 35 36 37 41 42
                 43 44 45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 44
                 45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                 44 47 51 52 53 54 55 56 57 63 64 65 73 74 75)
                (13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                 44 45 46 47 51 52 53 55 56 57 63 65 73 74 75)))
(check-expect (neighbours sample sample/init)
              '((24) (21)))
(check-expect (neighbours '(9 ()) '(11 99)) empty)
(check-expect (neighbours '(7 ()) '(43 44)) '((45) (42)))
(check-expect (neighbours '(7 ()) '(25 35)) '((45) (15)))
;;********************************************************************
;; (possible-moves board state peg-pos other-moves) determines
;;     all the possible moves a peg at a given position (peg-pos)
;;     can do or if it can even do anything. For example,
;;     if the peg at the given position is able to jump a peg
;;     to its right then it jumps it and returns a new state.
;;     If it can move right and left, the function produces two
;;     new states. If there's no possible moves the peg can do,
;;     the function will return empty. The function takes a
;;     Board, a State, a Peg (peg-pos), and a list (other-moves)
;;     The list acts as an accumulator that accumulates previous moves
;;     so that previous moves are not repeated.
;; possible-moves: Board State Peg (listof Any) -> (listof State)
;; Examples:
(check-expect (possible-moves '(5 ()) '(22 23 24) 23 '())
              '((21 24) (22 25)))
(check-expect (possible-moves '(6 ()) '(34 43 44 45 54) 44 '())
              '((24 43 45 54) (34 42 45 54)
                (34 43 46 54) (34 43 45 64)))

(define (possible-moves board state peg-pos other-moves)
  (local [;; (insert peg-pos state) inserts a given peg (peg-pos) into
          ;;     the state in the correct ascending order.
          ;; insert: Peg State -> State
          (define (insert peg-pos state)
            (append (filter (lambda (x) (< x peg-pos)) state)
                    (list peg-pos)
                    (filter (lambda (x) (> x peg-pos)) state)))

          ;; (assess state peg-pos direction other-moves) assess
          ;;     a possible move and determines whether or not it can
          ;;     be done or not. The function takes a State, Peg (peg-pos),
          ;;     a direction (direction) and a list of past moves
          ;;     (other-moves) so that neighbours doesn't repeat any move
          ;;     that it has already done.
          ;; assess: State Peg Int (anyof empty (listof Nat)) -> Bool
          (define (assess state peg-pos direction other-moves)
            (and (member? (+ peg-pos direction) state)
                 (not (member? (+ peg-pos (* 2 direction)) state))
                 (not (member? (+ peg-pos (* 2 direction)) (second board)))                      
                 (not (member? (+ peg-pos direction) other-moves))))]

    (cond [(and (assess state peg-pos (- 10) other-moves)
                (< 10 (- peg-pos 20)))
           (local [(define new-state (remove peg-pos (remove (- peg-pos 10) state)))]
             (cons (insert (- peg-pos 20) new-state)
                   (possible-moves board state peg-pos (cons (- peg-pos 10) other-moves))))]
          
          [(and (assess state peg-pos (- 1) other-moves)
                (< 0 (remainder (- peg-pos 2) 10)))
           (local [(define new-state (remove peg-pos (remove (- peg-pos 1) state)))]
             (cons (insert (- peg-pos 2) new-state)
                   (possible-moves board state peg-pos (cons (- peg-pos 1) other-moves))))]
          
          [(and (assess state peg-pos 1 other-moves)
                (> (+ 1 (first board)) (remainder (+ peg-pos 2) 10)))
           (local [(define new-state (remove peg-pos (remove (+ peg-pos 1) state)))]
             (cons (insert (+ peg-pos 2) new-state)
                   (possible-moves board state peg-pos (cons (+ peg-pos 1) other-moves))))]
          
          [(and (assess state peg-pos 10 other-moves)
                (< (+ peg-pos 20) (* (+ 1 (first board)) 10)))
           (local [(define new-state (remove peg-pos (remove (+ peg-pos 10) state)))]
             (cons (insert (+ peg-pos 20) new-state)
                   (possible-moves board state peg-pos (cons (+ peg-pos 10) other-moves))))]
          [else empty])))

;; Tests:
(check-expect (possible-moves '(5 ()) '(11 22 33 44 55) 11 '())
              empty)
(check-expect (possible-moves '(4 (21 24)) '(22 23 31 34 44) 23 '())
              empty)
(check-expect (possible-moves '(4 (21 24)) '(22 23 31 34 44) 34 '())
              empty)
(check-expect (possible-moves '(3 (31 33)) '(12 13 22) 12 '())
              '((13 32)))
(check-expect (possible-moves '(3 (31 33)) '(12 13 22) 13 '())
              '((11 22)))
(check-expect (possible-moves '(9 ()) '(15 25 35 45 55 65 75 85 95) 55 '())
              empty)
(check-expect (possible-moves '(9 ()) '(15 25 35 45 55 65 75 85 95 56) 56 '())
              '((15 25 35 45 54 65 75 85 95)))
;;********************************************************************
;; (solitaire board state sol) is the solver for the peg solitaire game.
;;     It consumes a Board, a State, and a Solution.
;;     If a Solution (sol) exists, the function produces a (listof State)
;;     that corresponds to a sequence of state starting with the inital
;;     state and ending with the solution state. The function will
;;     produce false if no solutions exists.
;; solitaire: Board State Solution -> (anyof false (listof State))
;; Examples:
(check-expect (solitaire '(4 ()) '(11 14 22 24 33) 'any)
              '((11 14 22 24 33)
                (11 22 33 34)
                (11 22 32)
                (11 12)
                (13)))
(check-expect (solitaire '(6 (11 12 16 22 32 43 44 46)) '(15 23 24 26
                                                             33 34 36) 'any)
              '((15 23 24 26 33 34 36)
                (15 25 26 33 34 36)
                (15 24 33 34 36)
                (15 24 35 36)
                (15 24 34)
                (14 15)
                (13)))

(define (make-neighbours board)
  (lambda (state) (neighbours board state)))

(define (solitaire board initial-state sol)
  (find-route initial-state (make-neighbours board) (make-solved? sol)))
   
;; Tests:
(check-expect (solitaire cross cross/greek 'any)
              '((24 34 42 43 44 45 46 54 64)
                (14 42 43 44 45 46 54 64)
                (14 34 42 43 45 46 64)
                (14 34 44 45 46 64)
                (14 24 45 46 64)
                (34 45 46 64)
                (34 44 64)
                (54 64)
                (74)))

(check-expect (solitaire cross cross/submarine 'any) '((34 42 43 44 45 46)
                                                       (42 43 45 46 54)
                                                       (44 45 46 54)
                                                       (34 45 46)
                                                       (34 44)
                                                       (54)))
(check-expect (solitaire '(5 (35)) '(11 13 15 22 24 31 33 35
                                     42 44 51 53 55) 'any) false)
(check-expect (solitaire '(3 ()) '(11 12) 'any)
              (list '(11 12) '(13)))
(check-expect (solitaire '(3 ()) '(11 12) 13)
              (list '(11 12)
                    '(13)))
;;********************************************************************
;; (result->text board result) helps visualize moves steps.
;;     The function takes a Board and a solution from the
;;     solitaire function (result) and produces visualization
;;     of the steps to solve the game or no-solution-text if
;;     there is no solution to the board.
;; result->text: Board (Board State Solution -> (anyof false (listof State))) -> (listof (listof Str))
;; Examples:
(check-expect (result->text '(4 ()) (solitaire '(4 ()) '(11 14 22 24 33) 'any))
              '(("O..O"
                 ".O.O"
                 "..O."
                 "....")
                ("O..."
                 ".O.."
                 "..OO"
                 "....")
                ("O..."
                 ".O.."
                 ".O.."
                 "....")
                ("OO.."
                 "...."
                 "...."
                 "....")
                ("..O."
                 "...."
                 "...."
                 "....")))
(check-expect (result->text '(4 ()) (solitaire '(4 ()) '(11 21 41) 21))
              '(("O..."
                 "O..."
                 "...."
                 "O...")
                ("...."
                 "...."
                 "O..."
                 "O...")
                ("...."
                 "O..."
                 "...."
                 "....")))

(define (result->text board result)
  (cond [(false? result) no-solution-text] 
        [(empty? result) empty]
        [else 
         (cons (state->los board (first result))
               (result->text board (rest result)))]))
    
;; Tests:
(check-expect (result->text '(3 ()) (solitaire '(3 ()) '(11 12) 'any))
              (list '("OO."
                      "..."
                      "...")
                    '("..O"
                      "..."
                      "...")))
(check-expect (result->text '(2 ()) (solitaire '(2 ()) '(11 12 21) 'any))
              no-solution-text)
(check-expect (result->text '(4 (41 42 43 44)) (solitaire '(4 ()) '(13 14 23 24 33) 'any))
              '(("..OO" 
                 "..OO"
                 "..O." 
                 "    ")
                (".O.."
                 "..OO"
                 "..O."
                 "    ")
                (".O.."
                 ".O.."
                 "..O."
                 "    ")
                ("...."
                 "...."
                 ".OO."
                 "    ")
                ("...."
                 "...." 
                 "...O"
                 "    ")))
(check-expect (result->text '(5 (21 22 24 14 31 32 41 42 43 44 45))
                           (solitaire '(5 (21 22 24 14 31 32 42 43 44 45)) '(11 12 23 34 25) 'any))
              '(("OO. ."
                 "  O O"
                 "  .O."
                 "     "
                 ".....")
                ("..O ."
                 "  O O"
                 "  .O."
                 "     "
                 ".....")
                ("... ."
                 "  . O"
                 "  OO."
                 "     "
                 ".....")
                ("... ."
                 "  . O"
                 "  ..O"
                 "     "
                 ".....")
                ("... O"
                 "  . ."
                 "  ..."
                 "     "
                 ".....")))
;;********************************************************************
;; The following have all been ran with my program and all work
;;cross/big-diamond takes approximately
;;     real time: 4185
;;cross/init takes approximately
;;     real time: 4876
;;********************************************************************
;(show (result->text cross (solitaire cross cross/submarine 'any)))
;(show (result->text cross (solitaire cross cross/greek 'any)))
;(show (result->text cross (solitaire cross cross/small-diamond 'any)))
;(time (empty? (result->text cross (solitaire cross cross/big-diamond 'any))))
;(show (result->text cross (solitaire cross cross/pyramid-16 'any)))
;(show (result->text '(5 ()) (solitaire '(5 ()) five-x-five 'any)))
;(show (result->text '(4 ()) (solitaire '(4 ()) '(12 13  
;                                                    21 22 23 24
;                                                    31 32 33 34
;                                                    42 43) 'any)))
;(time (empty? (result->text cross (solitaire cross cross/init 'any))))
;;********************************************************************
;; This is a provided function: no additional documentation required
;; Uncomment the following line after your neighbours is complete and tested

;; try this when you are done: (but leave it commented out when you submit)
; (show (result->text cross (solitaire cross cross/init 'any)))

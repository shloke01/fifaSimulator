#lang racket
(require csc151)
(require plot)

;;; File:
;;;   Final-project-code

;;; Authors:
;;;   Shaurya Saboo
;;;   Sagnik Ghosh
;;;   Shloke Meresh

;;; Contents:
;;;   Code for the CSC151 class project

;-------------------------------------------------------------------------------------------------------------------------------------------

(define data
  (drop (read-csv-file "/users/your-username-here/Desktop/fifa19data.csv") 1))

;----------|
;Program 1 |
;----------|

;;; Procedure:
;;;   player-pos
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find the position of a single player in the fifa19 dataset
;;; Produces:
;;;   pos, a string
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   pos will be the position of the player in lst

(define player-pos
  (lambda (lst)
    (list-ref lst 10)))

;;; Predicate:
;;;   goalkeeper?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find whether the player in lst is a goalkeeper or not
;;; Produces:
;;;   result, a boolean
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   #t if the player is a goalkeeper
;;;   #f if the player is not a goalkeeper

(define goalkeeper?
  (lambda (lst)
    (equal? "GK\r" (player-pos lst))))

;;; Predicate:
;;;   defender?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find whether the player in lst is a defender or not
;;; Produces:
;;;   result, a boolean
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   #t if the player is a defender
;;;   #f if the player is not a defender

(define defender?
  (lambda (lst)
    (or (equal? "CB\r" (player-pos lst))
        (equal? "LCB\r" (player-pos lst))
        (equal? "RCB\r" (player-pos lst))
        (equal? "LB\r" (player-pos lst))
        (equal? "RB\r" (player-pos lst))
        (equal? "LWB\r" (player-pos lst))
        (equal? "RWB\r" (player-pos lst)))))

;;; Predicate:
;;;   midfielder?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find whether the player in lst is a midfielder or not
;;; Produces:
;;;   result, a boolean
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   #t if the player is a midfielder
;;;   #f if the player is not a midfielder

(define midfielder?
  (lambda (lst)
    (or (equal? "CM\r" (player-pos lst))
        (equal? "LCM\r" (player-pos lst))
        (equal? "RCM\r" (player-pos lst))
        (equal? "LM\r" (player-pos lst))
        (equal? "RM\r" (player-pos lst))
        (equal? "CDM\r" (player-pos lst))
        (equal? "LDM\r" (player-pos lst))
        (equal? "RDM\r" (player-pos lst))
        (equal? "CAM\r" (player-pos lst))
        (equal? "LAM\r" (player-pos lst))
        (equal? "RAM\r" (player-pos lst))
        (equal? "LW\r" (player-pos lst))
        (equal? "RW\r" (player-pos lst)))))

;;; Predicate:
;;;   forward?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find whether the player in lst is a forward or not
;;; Produces:
;;;   result, a boolean
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   #t if the player is a forward
;;;   #f if the player is not a forward

(define forward?
  (lambda (lst)
    (or (equal? "ST\r" (player-pos lst))
        (equal? "RS\r" (player-pos lst))
        (equal? "LS\r" (player-pos lst))
        (equal? "CF\r" (player-pos lst))
        (equal? "LF\r" (player-pos lst))
        (equal? "RF\r" (player-pos lst)))))

;;; Procedure:
;;;   goalkeepers
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To make a list of all the goalkeepers in the fifa19 dataset
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   lst is a list of lists containing all the goalkeepers in data
    
(define goalkeepers
  (filter goalkeeper?
          data))

;;; Procedure:
;;;   defenders
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To make a list of all the defenders in the fifa19 dataset
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   lst is a list of lists containing all the defenders in data

(define defenders
  (filter defender?
          data))

;;; Procedure:
;;;   midfielders
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To make a list of all the midfielders in the fifa19 dataset
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   lst is a list of lists containing all the midfielders in data

(define midfielders
  (filter midfielder?
          data))

;;; Procedure:
;;;   forwards
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To make a list of all the forwards in the fifa19 dataset
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   lst is a list of lists containing all the forwards in data

(define forwards
  (filter forward?
          data))

;;; Procedure:
;;;   player-rating
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   To find the rating of a single player in the fifa19 dataset
;;; Produces:
;;;   rating, a positive integer
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   rating is the rating of the player in lst

(define player-rating
  (lambda (lst)
    (list-ref lst 5)))

;;; Procedure:
;;;   goalkeeper-rating-histogram
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To create a histogram of goalkeeper ratings
;;; Produces:
;;;   hist, a histogram
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   hist is a discrete histogram of the tallies of all goalkeeper ratings in data

(define goalkeeper-rating-histogram
  (parameterize ([plot-x-tick-label-angle -50])
    (plot (discrete-histogram (tally-all (map player-rating goalkeepers)))
          #:title "Goalkeeper Ratings"
          #:x-label "Rating"
          #:y-label "Frequency"))) ;mode 63

;;; Procedure:
;;;   defender-rating-histogram
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To create a histogram of defender ratings
;;; Produces:
;;;   hist, a histogram
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   hist is a discrete histogram of the tallies of all defender ratings in data

(define defender-rating-histogram
  (parameterize ([plot-x-tick-label-angle -50])
    (plot (discrete-histogram (tally-all (map player-rating defenders)))
          #:title "Defender Ratings"
          #:x-label "Rating"
          #:y-label "Frequency"))) ;mode 64

;;; Procedure:
;;;   midfielder-rating-histogram
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To create a histogram of midfielder ratings
;;; Produces:
;;;   hist, a histogram
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   hist is a discrete histogram of the tallies of all midfielder ratings in data

(define midfielder-rating-histogram
  (parameterize ([plot-x-tick-label-angle -50])
    (plot (discrete-histogram (tally-all (map player-rating midfielders)))
          #:title "Midfielder Ratings"
          #:x-label "Rating"
          #:y-label "Frequency"))) ;mode 66

;;; Procedure:
;;;   forward-rating-histogram
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   To create a histogram of forward ratings
;;; Produces:
;;;   hist, a histogram
;;; Preconditions:
;;;   [No Additional]
;;; Postconditions:
;;;   hist is a discrete histogram of the tallies of all forward ratings in data

(define forward-rating-histogram
  (parameterize ([plot-x-tick-label-angle -50])
    (plot (discrete-histogram (tally-all (map player-rating forwards)))
          #:title "Forwards Ratings"
          #:x-label "Rating"
          #:y-label "Frequency"))) ;mode 67


;-------------------------------------------------------------------------------------------------------------------------------------------

;----------|
;Program 2 |
;----------|

;;; Predicate:
;;;   right-footed?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   Checks whether or not a player is right footed 
;;; Produces:
;;;   result, a boolean  
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions
;;;   #t if the player in lst is right-footed
;;;   #f if the player in lst is left-footed

(define right-footed?
  (lambda (lst)
    (equal? (list-ref lst 9) "Right")))


;;; Predicate:
;;;   left-footed?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   Checks whether or not a player is left footed 
;;; Produces:
;;;  result, a boolean
;;; Preconditions:
;;;  lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions
;;;   #t if the player in lst is left-footed
;;;   #f if the player in lst is right-footed

(define left-footed?
  (lambda (lst)
    (equal? (list-ref lst 9) "Left")))

;;; Procedure:
;;;   right-footed-players
;;; Parameters:
;;;   [none]
;;; Purpose:
;;;   To create a list of right footed players
;;; Produces:
;;;  lst, a list
;;; Preconditions:
;;;  [No Additional]
;;; Postconditions
;;;  lst is a list of lists containing all the right-footed players in data

(define right-footed-players
  (filter right-footed?
          data))

;;; Procedure:
;;;   left-footed-players
;;; Parameters:
;;;   [none]
;;; Purpose:
;;;   To create a list of left footed players
;;; Produces:
;;;  lst, a lsit
;;; Preconditions:
;;;  [No Additional]
;;; Postconditions
;;;  lst is a list of lists containing all the left-footed players in data

(define left-footed-players
  (filter left-footed?
          data))

;;; Procedure:
;;;   right-foot-avg-rating
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Calculates the average rating of right footed players 
;;; Produces:
;;;   num, a number
;;; Preconditions:
;;;  [No Additional]
;;; Postconditions
;;;   num will be the average rating of right footed players

(define right-foot-avg-rating
  (let ([list-of-ratings (map (section list-ref <> 5) right-footed-players)]) ;making a list of the ratings of all right-footed players
    (exact->inexact
     (/ (reduce + list-of-ratings)    
        (length list-of-ratings)))))  ;summing and dividing by total to find average

;;; Procedure:
;;;   left-foot-avg-rating
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Calculates the average rating of left footed players 
;;; Produces:
;;;;  num, a number
;;; Preconditions:
;;;  [No Additional]
;;; Postconditions
;;;  num will be the average rating of left footed players

(define left-foot-avg-rating
  (let ([list-of-ratings (map (section list-ref <> 5) left-footed-players)]) ;making a list of the ratings of all left-footed players
    (exact->inexact
     (/ (reduce + list-of-ratings)
        (length list-of-ratings)))))  ;summing and dividing by total to find average


;;; Procedure:
;;;   compare-left-and-right
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Compares which group of players (left footed or right footed players) has a higher average rating
;;; Produces:
;;;   str, a string 
;;; Preconditions:
;;;  [No Additional]
;;; Postconditions:
;;;   The ouput must be one of the three strings:
;;;   * "Right footed players have a higher average rating than left footed players"
;;;   * "Left footed players have a higher average rating than right footed players"
;;;   * "Right-footed and left-footed players have the same average rating"

(define compare-left-and-right
  (cond [(> right-foot-avg-rating left-foot-avg-rating)
         "Right footed players have a higher average rating than left footed players"]
        [(< right-foot-avg-rating left-foot-avg-rating)
         "Left footed players have a higher average rating than right footed players"]
        [(= right-foot-avg-rating left-foot-avg-rating)
         "Right-footed and left-footed players have the same average rating"]))


;-------------------------------------------------------------------------------------------------------------------------------------------

;----------|
;Program 3 |
;----------|

;;; Predicate:
;;;   FCB-player?
;;; Parameters:
;;;   lst, a non-empty list 
;;; Purpose:
;;;   Checks whether or not the player in lst plays for FC Barcelona
;;; Produces:
;;;   val, a boolean value
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   * returns #t when an element of the list
;;;   is "FC Barcelona"
;;;   * Else, returns #f

(define FCB-player?
  (lambda (lst)
    (equal? (list-ref lst 6) "FC Barcelona")))

;;; Predicate:
;;;   RMA-player?
;;; Parameters:
;;;   lst, a non-empty list 
;;; Purpose:
;;;   Checks whether or not the player in lst plays for Real Madrid
;;; Produces:
;;;   val, a boolean value
;;; Preconditions:
;;;   lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   * returns #t when an element of the list
;;;   is "Real Madrid"
;;;   * Else, returns #f

(define RMA-player?
  (lambda (lst)
    (equal? (list-ref lst 6) "Real Madrid")))

;;; Procedure:
;;;   barcelona-players
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Filters the players from FCB with the help of predicate FCB?
;;; Produces:
;;;   lst1, a list
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   lst1 should be a list which contains
;;;   players only from "FC Barcelona"

(define barcelona-players
  (filter FCB-player? data))

;;; Procedure:
;;;   realmadrid-players
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Filters the players from RMA with the help of predicate RMA?
;;; Produces:
;;;   lst1, a list
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   lst1 should be a list which contains
;;;   players only from "Real Madrid"
 
(define realmadrid-players
  (filter RMA-player? data))

;;; Procedure:
;;;   random-elt
;;; Parameters:
;;;   lst, a non-empty list 
;;; Purpose:
;;;   Unpredictably pick an element of lst.
;;; Produces:
;;;   val, a value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * val is an element of lst.
;;;   * If lst contains more than one element, it is difficult to predict 
;;;     which element val is.

(define random-elt
  (lambda (lst)
    (list-ref lst (random (length lst)))))

;;; Procedure:
;;;   get-random-rating
;;; Parameters:
;;;   lst, a list of lists
;;; Purpose:
;;;   To selct a random player from a list and get its rating
;;; Produces:
;;;   rating, a number
;;; Preconditions:
;;;  lst must be a list containg lists in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   * rating is an element of one of the lists within lst
;;;   * If lst contains more than one list, it is difficult to predict 
;;;     which list rating belongs to.

(define get-random-rating
  (lambda (lst)
    (list-ref (random-elt lst) 5)))

;;; Procedure:
;;;   team-name
;;; Parameters:
;;;   lst, a non empty list
;;; Purpose:
;;;   To extract the name of the club that the player in lst belongs to
;;; Produces:
;;;   team, a string
;;; Preconditions:
;;;  lst must be in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   * team is an element of lst.
;;;   * team is a string

(define team-name
  (lambda (lst)
    (list-ref (car lst) 6)))

;;; Procedure:
;;;   simulated-match
;;; Parameters:
;;;   team1, a list of lists
;;;   team2, a list of lists
;;; Purpose:
;;;   To simulate a match of two teams
;;;   With 11 battles between players of
;;;   both teams based on ratings
;;; Produces:
;;;   sim, a string
;;; Preconditions:
;;;  team1 and team2 must be lists containing lists in the required format ("S.No" "ID" "Name" "Age" "Nationality" "Overall" "Club" "Value" "Wage" "Preferred Foot" "Position\r")
;;; Postconditions:
;;;   if the match is a tie, sim will be "The match resulted in a tie"
;;;   else, sim will be
;;;   "(winner of match) wins the match" 
 
(define simulated-match
  (lambda (team1 team2)
    (let kernel ([rating1 (get-random-rating team1)] ;getting a random rating from team1
                 [rating2 (get-random-rating team2)] ;getting a random rating from team2
                 [pts-team1 0]
                 [pts-team2 0]
                 [simulation 0])
      ;(display (list 'simulation simulation)) (newline)                           ;;; [                                                     ]
      ;(display (list 'pts-team1 pts-team1 'pts-team2 pts-team2)) (newline)        ;;; [      uncomment to see how the simulation works      ]
      ;(display (list 'rating1 rating1 'rating2 rating2)) (newline) (newline)      ;;; [                                                     ]
      (if (equal? simulation 11)  ;base case
          (cond
            [(> pts-team1 pts-team2)
             (string-append (team-name team1) " wins the match")]
            [(< pts-team1 pts-team2)
             (string-append (team-name team2) " wins the match")]
            [(= pts-team1 pts-team2)
             "The match resulted in a tie"])
          (cond
            [(> rating1 rating2)
             (kernel (get-random-rating team1) (get-random-rating team2) (+ 1 pts-team1) pts-team2 (+ 1 simulation))] ;if team1 wins the battle, increment pts-team1 and call the kernel
            [(< rating1 rating2)
             (kernel (get-random-rating team1) (get-random-rating team2) pts-team1 (+ 1 pts-team2) (+ 1 simulation))] ;if team2 wins the battle, increment pts-team2 and call the kernel
            [(= rating1 rating2)
             (kernel (get-random-rating team1) (get-random-rating team2) pts-team1 pts-team2 (+ 1 simulation))])))))  ;if there is a tie, call the kernel


;-------------------------------------------------------------------------------------------------------------------------------------------
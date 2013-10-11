
(setf *print-circle* t)

(defun gen-deck (&optional (num-jokers 2))
  (append
   (loop :for i :below num-jokers :collecting (list 'joker :joker))
   (loop :for num :in '(2 3 4 5 6 7 8 9 10 J Q K A) :appending
      (loop :for suit :in '(:clubs :spades :diamonds :hearts) :collecting
         (list num suit)))))

(defun rank (card)
  (car card))
(defun suit (card)
  (cadr card))

(defun shuffle-cards (deck)
  ;; This idiom is known as "decorate-sort-undecorate" in CL, in perl,
  ;; it is known as a "schwartzian transform".

  (mapcar
   ;; Pick out the cards again in the randomly sorted order: "undecorate"
   #'(lambda (xfrom) (cadr xfrom))
   ;; Produce a sorted list of cards according to a random numbe: "sort"
   (sort
    ;; First, assocate a random number with each card in the deck: "decorate"
    (mapcar #'(lambda (card) (list (random 1.0) card)) deck)
    ;; then sort in descending order
    #'<
    ;; by the random numbrs
    :key #'car)))

(defstruct meld
  ;; a list of cards in a meld
  cards)

(defstruct meld-set
  (id (gensym "MELD-SET-"))
  ;; list of meld structures
  melds)

(defstruct player
  (id (gensym "PLAYER-"))
  meld-set
  team
  hand
  foot)

(defstruct team
  (id (gensym "TEAM-"))
  (meld-set (make-meld-set))
  meld-point-threshold-attained
  players
  (points 0))

(defstruct game
  stock
  discard
  (point-limit 10000)
  teams
  (round 0)
  player-order
  (current-player 0)
  num-decks)

(defun create-stock (num)
  (shuffle-cards (loop :for i :below num :appending (gen-deck))))

;; first player name is a keyword that is a team name
(defun create-team (player-names)
  (let ((the-team (make-team :id (car player-names)))
        (player-names (cdr player-names))
        (players ()))
    (setf (team-players the-team)
          (dolist (player-name player-names (nreverse players))
            (push (make-player :id player-name
                               :team the-team
                               :meld-set (team-meld-set the-team))
                  players)))
    the-team))

(defun create-teams (&rest ps)
  (mapcar #'create-team ps))

(defun pop-n-cards (game &optional (num 7))
  (let ((cards ()))
    (dotimes (i num (nreverse cards))
      (push (pop (game-stock game)) cards))))


(defun gen-new-game (num-decks &rest teams)
  (let* ((game (make-game :num-decks num-decks
                          :teams (apply #'create-teams teams))))

    (setf (game-player-order game)
          ;; Create a single vector containing the individual players
          (apply #'vector
                 ;; Take a player from each individual list and assemble them
                 ;; into a new list, appending them together into a single
                 ;; list.
                 (apply #'mapcan
                        #'(lambda (&rest p)
                            (copy-seq p))
                        ;; Create a new list of individual lists of
                        ;; players from each team. We copy seq it
                        ;; because we'll be destructively modifying
                        ;; the result.
                        (copy-seq (mapcar
                                   #'(lambda (team)
                                       (team-players team))
                                   (game-teams game))))))

    ;; Pick a random player who starts the round.
    (setf (game-current-player game)
          (random (length (game-player-order game))))

    game))


(defmacro walk-players ((psym gsym game) &body body)
  (let ((index (gensym)))
    `(funcall
      (lambda (,gsym)
        (dotimes (,index (length (game-player-order ,gsym)))
          (let ((,psym (aref (game-player-order ,gsym) ,index)))
            ,@body)))
      ,game)))

(defmacro walk-teams ((tsym gsym game) &body body)
  `(funcall
    (lambda (,gsym)
      (dolist (,tsym (game-teams ,gsym))
        ,@body))
    ,game))

;; TODO handle what happens if we run out of cards from the stock.
(defun reset-cards (game &optional (number-of-cards 11))
  (setf (game-stock game) (create-stock (game-num-decks game)))
  (setf (game-discard game) (pop-n-cards game 1))

  ;; Blow away all of the melds
  (walk-teams (team g game)
    (setf (team-meld-point-threshold-attained team) nil)
    (setf (meld-set-melds (team-meld-set team)) nil))

  ;; deal the hand and foot
  (walk-players (p g game)
    (setf (player-hand p) nil)
    (dolist (c (reverse (pop-n-cards game number-of-cards)))
      (push c (player-hand p)))

    (setf (player-foot p) nil)
    (dolist (c (reverse (pop-n-cards game number-of-cards)))
      (push c (player-foot p)))))

;; return true if the player's hand and foot are empty
(defun outp (player)
  (and (null (player-hand player))
       (null (player-foot player))))

(defun current-player (game)
  (aref (game-player-order game) (game-current-player game)))

(defun next-player (game)
  "when this is called, the next player becomes active in the game structure"
  (setf (game-current-player game)
        (mod (1+ (game-current-player game))
             (length (game-player-order game)))))

(defun score-card (card location)
  (destructuring-bind (rank suit) card
    (cond
      ((eq rank 'joker) 50)
      ((eql rank 2) 20)
      ((eq rank 'a) 20)
      ((intersection (list rank) '(k q j 10 9 8)) 10)
      ((intersection (list rank) '(7 6 5 4)) 5)
      ((and (eql rank 3) (or (eq suit :hearts) (eq suit :diamonds)))
       (case location
         ((:hand :foot)
          ;; This will ultimately get subtracted in score-a-round!
          300)
         (:meld
          100)
         (t 0)))
      ;; See red three comment for why this is positive.
      ((and (eql rank 3) (or (eq suit :clubs) (eq suit :spades))) 5)
      (t
       (error "Fix score-card for ~A~%" card)))))

(defun meld-canasta-p (meld)
  (>= (length (meld-cards meld)) 7))

(defun meld-dirty-p (meld)
  (or (member 2 (meld-cards meld) :key #'car)
      (member 'joker (meld-cards meld) :key #'car)))

(defun compute-meld-set-score (meld-set)
  (let ((meld-score 0))
    (when meld-set
      (dolist (a-meld meld-set)
        ;; score clean or dirty
        (incf meld-score (+ (if (meld-canasta-p a-meld)
                                (if (meld-dirty-p a-meld)
                                    300
                                    500)
                                0)))
        ;; Score each card in the meld regardless of cleanliness
        (dolist (a-card (meld-cards a-meld))
          (incf meld-score (score-card a-card :meld)))))
    meld-score))

(defun score-a-round (game)
  ;; for each team:
  ;; A: score cards in all players' hands
  ;; B: score cards in all players' feet

  ;; C: score canasta completion
  ;; D: score meld cards individually
  ;; score = (C + D) - (A + B)

  (walk-teams (team g game)
    (flet ((compute-hand/foot-score (pick-function hand/foot)
             (- (reduce
                 #'+ (mapcan #'(lambda (p)
                                 (mapcar #'(lambda (c)
                                             (score-card c hand/foot))
                                         (funcall pick-function p)))
                             (team-players team))))))
      (incf (team-points team)
            (+
             ;; A (remember it comes out negative!)
             (compute-hand/foot-score #'player-hand :hand)
             ;; B (remember it comes out negative!)
             (compute-hand/foot-score #'player-foot :foot)
             ;; C and D
             (compute-meld-set-score
              (meld-set-melds (team-meld-set team))))))))


(defun estimate-card-pile-size (stock total-cards)
  (let ((stock-size-percent (/ (length stock) total-cards)))
    (cond
      ((< stock-size-percent .3)
       "small")
      ((and (< stock-size-percent .6) (>= stock-size-percent .3))
       "medium")
      (t
       "large"))))

(defun short-hand-name (card)
  (if (null card)
      "empty"
      (destructuring-bind (rank suit) card
        (format nil "~A~A"
                ;; suitable rank specifer
                (etypecase rank
                  (number rank)
                  (symbol (if (eq rank 'joker)
                              "Jk"
                              (aref (symbol-name rank) 0))))

                ;; suitable suit specifer (just the first letter of the suit)
                (aref (symbol-name suit) 0)))))

(defun get-meld-type (meld)
  "return 0 if only a meld, 1 if a canasta, and 2 if a canasta with a
red 3 on it"
  (if (meld-canasta-p meld)
      (if (member 3 (meld-cards meld) :key #'car)
          2
          1)
      0))

(defun display-team-status (team)
  (format t "Team ~A [~A]: ~{ ~{ ~A ~A/~A~} ~}~%"
          (team-id team)
          (team-points team)
          (loop for player in (team-players team)
             collect (list (player-id player)
                           (length (player-hand player))
                           (length (player-foot player))))))


(defun display-team-meld-ranks (team)
  "display rank count for each meld"
  (format t "C ~{  ~A  ~}~%"
          (loop for meld in (meld-set-melds (team-meld-set team))
             collect (count (car (meld-cards meld)) (meld-cards meld)))))


(defun display-team-meld-2-count (team)
  "display the 2 count for each meld"
  (format t "2 ~{  ~A  ~}~%"
          (loop for meld in (meld-set-melds (team-meld-set team))
             ;; TODO: write a function to find the rank given a meld
             ;; (in case first card is JkJ or 2)
             collect (count 2 (mapcar 'car (meld-cards meld))))))


(defun display-team-meld-joker-count (team)
  "display the Joker count for each meld"
  (format t "J ~{  ~A  ~}~%"
          (loop for meld in (meld-set-melds (team-meld-set team))
             ;; TODO: write a function to find the rank given a meld
             ;; (in case first card is JkJ or 2)
             collect (count 'joker (mapcar 'car (meld-cards meld))))))

(defun compute-team-play-down-points (team)
  (cond
    ((<= (team-points team) 2500)
     50)
    ((<= (team-points team) 5000)
     90)
    ((<= (team-points team) 7500)
     120)
    (t 150)))

(defun display-team-meld-set (team)
  ;; display meld rank and status (Is it a canasta? Is there a red three?)
  (if (meld-set-melds (team-meld-set team))
      (progn
        (format t "R ~{~[ ~;+~;*~]~:[[~;{~]~A~:[]~;}~] ~}~%"
                (loop :for meld :in (meld-set-melds (team-meld-set team))
                   :append (list (get-meld-type meld)
                                 (meld-dirty-p meld)
                                 (caar (meld-cards meld))
                                 (meld-dirty-p meld))))
        (display-team-meld-ranks team)
        (display-team-meld-2-count team)
        (display-team-meld-joker-count team))
      (format t " Play Down Points: ~A points~%"
              (compute-team-play-down-points team))))

(defun current-player-p (player game)
  (eq player (current-player game)))

(defun display-player-list (game)
  (format t "Turn: ~{~:[~;[~]~A~:[~;]~] ~}~%"
          (loop :for player :across (game-player-order game)
             :append (list (current-player-p player game)
                           (player-id player)
                           (current-player-p player game)))))

;; This is a dumb function, maybe there is a better way to do this.
(defun sort-rank (r)
  "When we sort ranks, use this order"
  (let ((sr '((2 . 0)
              (3 . 1)
              (4 . 2)
              (5 . 3)
              (6 . 4)
              (7 . 5)
              (8 . 6)
              (9 . 7)
              (10 . 8)
              (J . 9)
              (Q . 10)
              (K . 11)
              (A . 12)
              (joker . 13))))
    (cdr (assoc r sr))))

(defun generate-card-key (card)
  "Create a unique and repeatable hash key for this card. The key is a
little special since it also encode the sorting position of the key in
relation to another key."

  (destructuring-bind (rank suit) card
    (list
     (ecase rank
       ((2 4 5 6 7 8 9 10 J Q K A)
        (format nil "~A" rank))
       (3
        (format nil "~A~A" rank
                (ecase suit
                  ((:spades :clubs)
                   "B")
                  ((:diamonds :hearts)
                   "R"))))
       (joker
        "Jk"))

     ;; and decorate the key with the sorting value so we can order
     ;; the keys later.
     (sort-rank rank))))

(defun collate-players-hand (player &optional additional-card-pile)
  "Count the cards participating in the player's hand along with any
addition cards that could potentially be in the player's hand"
  (let ((h (make-hash-table :test #'equal))
        (all-cards (append (copy-list (player-hand player))
                           (copy-list additional-card-pile))))
    (mapcar #'(lambda (card)
                (let ((key (generate-card-key card)))
                  (multiple-value-bind (value present) (gethash key h)
                    (declare (ignore value))
                    (if present
                        (incf (gethash key h))
                        (setf (gethash key h) 1)))))
            all-cards)
    h))

(defun display-current-players-hand (game &optional picked)
  (format t "        Hand~:[*~;~]~%" picked)
  (let* ((collation (collate-players-hand (current-player game)))
         (display-fragments nil))

    (maphash #'(lambda (card-key amount)
                 (push (list card-key amount)
                       display-fragments))
             collation)

    ;; Remember, SORT is destructive.
    (setf display-fragments (sort display-fragments #'< :key #'cadar))

    (format t "~{~<~%~1,72:;~8A~>~}~%"
            (mapcar #'(lambda (fragment)
                        (destructuring-bind ((rank sort-rank) count) fragment
                          (declare (ignore sort-rank))
                          (format nil "~A(~A)" rank count)))
                    display-fragments))))


(defun display-turn (game &optional first)
  ;; Show stock and discard
  (format t "Stock: ~A~%" (estimate-card-pile-size (game-stock game)
                                                   (game-num-decks game)))
  (format t "Discard: ~A ~A~%" (short-hand-name (first (game-discard game)))
          (estimate-card-pile-size (game-discard game) (game-num-decks game)))
  ;; show team meld-sets
  (walk-teams (team g game)
    (format t "---~%")
    (display-team-status team)
    (display-team-meld-set team)
    (format t "---~%~%"))
  ;; show player list noting current player
  (display-player-list game)
  ;; show current player's hand
  (display-current-players-hand game first)
  )

(defun format-finish (strm fmt &rest args)
  (apply #'format strm fmt args)
  (finish-output))

;; return a (values t/nil :reason-code)
(defun playdown-p (game normalized-card-counts &optional discard-pile)
  #|
  (macrolet ((threshold-value (g)
  `(team-meld-point-threshold-attained
  (player-team (current-player ,g)))))
  |#

  ;; get a summary of the player's hand as a hash table
  (let* ((top-discard (car discard-pile))
         (discard-pile (if top-discard (cdr discard-pile) discard-pile))
         (normalized-card-counts
          (if top-discard
              ;; XXX increase normlized cards by one if already present,
              ;; otherwise do the below
              (append `((1 ,(rank top-discard)) normalized-card-counts))
              normalized-card-counts)))
    (collation (collate-players-hand (current-player game) discard-pile)))
  ;; 1. First thing we do is see if the player's hand can satisfy the
  ;; needs of the playdown.
  (maphash #'(lambda (card-key amount)
               ;; (count rank)
               ;; ((2 4) (1 5) (5 6)) -> normalized-card-counts
               ;; (rank count)
               ;; ((4 6) (5 3) (6 2) (joker 3)) -> collation
               (let ((in-hand (member card-key normalized-card-counts
                                      :key 'second)))
                 (when in-hand
                   (unless (>= amount (caar in-hand))
                     (return (values nil "You don't posses some of these playdown cards in your hand!"))))))
           collation)

  ;; 2. If there is a top-discard we need to satisfy, ensure it is in the
  ;; normalized-card-counts
  (when top-discard
    ;;
    (member (rank top-discard) normalized-card-counts :key 'second)

    ;; B. We either must have a meld already present that can accept
    ;; the card and red 3's can only be melded on melds with certain
    ;; properties.
    42)


  (let (cards-present-in-hand-p (cards-in-hand normalized-cards game)

                                (passed-threshold-p
                                 (team-meld-point-threshold-attained
                                  (player-team (current-player game))))

                                (score-good-p (>= score (compute-team-play-down-points
                                                         (player-team
                                                          (current-player game)))))



                                42)))

(defun score-playdown (game args)
  42)

(defun enact-playdown (game args)
  42)

(defun normalize-play-down-card (unormalized-card)
  (if (consp unormalized-card)
      unormalized-card
      (list 1 unormalized-card)))

(defun do-a-player-turn (game)
  ;; TODO!

  (format t "Do a player turn!~%")

  ;; loop untill done
  ;;  display hand
  ;;  take command
  ;; (outp (current-player game)))
  (let (done
        (picked nil))
    (loop :until done
       :do
       (format-finish t "************************************************~%")
       (display-turn game picked)
       (format-finish t "Command > ")
       (let ((form (read))) ;; revisit the read for EOL and shit
         (format-finish t "Read form: ~A~%" form)
         (cond
           ((symbolp form)
            (cond
              (t
               (format-finish t "Unknown symbolic command!~%"))))

           ((consp form)
            (destructuring-bind (command &rest args) form
              (cond
                ;; handle exiting
                ((or (eq command 'done) (eq command 'quit))
                 (setf done t))

                ;; handle picking 2 cards just from the stock
                ((eq command 'pick-stock)
                 (if picked
                     (format-finish t "~%You already picked!~%")
                     (progn
                       (dolist (c (reverse (pop-n-cards game 2)))
                         (push c (player-hand (current-player game))))
                       (setf picked t))))

                ((eq command 'pick-discard)
                 ;; perform a play-down mixed with a pickup of the discard.
                 ;; This is "atomic" in that if we discover a problem we can
                 ;; undo it and put all of the cards we picked up back into
                 ;; the discard pile.
                 42)

                ;; This is a regular play down
                ((eq command 'play-down)
                 (let ((normalized-card-counts
                        (mapcar 'normalize-play-down-card args)))
                   (multiple-value-bind (can-play-p reason)
                       (playdown-p game normalized-card-counts)
                     (if can-play-p
                         (let ((score (score-playdown game
                                                      normalized-card-counts)))
                           (enact-playdown game normalized-card-counts)
                           (setf (team-meld-point-threshold-attained
                                  (player-team (current-player game))) t))
                         (format-finish t "Illegal play down attempted: [~A]~%"
                                        reason)))))
                (t
                 (format-finish t "Unknown command!~%")))))


           (t
            (format-finish t "Bad command!%"))))))
  t)

(defun do-a-round (game)

  (format t "Doing a round!~%")

  (reset-cards game)

  (loop :until (do-a-player-turn game) :do (next-player game))

  (score-a-round game)

  ;; return the maximal points out of the teams.
  (reduce
   #'(lambda (&rest teams)
       (apply #'max (mapcar #'(lambda (team) (team-points team))
                            teams)))
   (game-teams game))

  ;; XXX force the game to finish after one round for testing purposes
  10000000000)

(defun hand-and-foot (num-decks &rest players)
  (let ((game (apply #'gen-new-game num-decks players)))

    (loop :until (>= (do-a-round game) (game-point-limit game)))

    ;; the winning team
    (format t "The winning team is ~A~%"
            (mapcar #'(lambda (p) (player-id p))
                    (team-players (player-team (current-player game)))))))

(defun doit ()
  (hand-and-foot 5 '(:warehos "ken" "jenn") '(:skinnykitty "pete" "steph")))


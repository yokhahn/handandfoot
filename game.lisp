
(setf *print-circle* t)

(defun gen-deck (&optional (num-jokers 2))
  (append
   (loop :for i :below num-jokers :collecting (list :joker :joker))
   (loop :for num :in '(2 3 4 5 6 7 8 9 10 J Q K A) :appending
      (loop :for suit :in '(:clubs :spades :diamonds :hearts) :collecting
         (list num suit)))))

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
  players
  (points 0))

(defstruct game
  (stock (create-stock 5))
  discard
  (point-limit 10000)
  teams
  (round 0)
  player-seating
  current-player)

(defun create-stock (num)
  (shuffle-cards (loop :for i :below num :appending (gen-deck))))

(defun create-team (player-names)
  (let ((the-team (make-team))
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

#|
(defmacro walk-players ((psym gsym game) &body body)
  (let ((pg (gensym))
        (g (gensym)))
    `(funcall
      (lambda (,gsym)
        (dolist (,pg (game-players ,gsym))
          (dolist (,psym ,pg)
            ,@body))
        ,gsym)
      ,game)))
|#

(defun gen-new-game ()
  (let* ((game (make-game :teams
                          (create-teams '("ken" "jenn") '("pete" "steph")))))

    (setf (game-player-seating game)
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
    game))


#|
(defun initial-deal-to-players (game)
  (walk-players (p g game)
    (dolist (c (reverse (pop-n-cards game)))
      (push c (player-hand p)))
    (dolist (c (reverse (pop-n-cards game)))
      (push c (player-foot p)))))
|#

    (defun game-donep (game)
      game
      )


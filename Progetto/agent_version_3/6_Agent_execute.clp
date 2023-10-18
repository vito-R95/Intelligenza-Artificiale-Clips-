;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT_EXECUTE (import AGENT_PLAN ?ALL) (export ?ALL))


; esegue una unguess
(defrule do_unguess 
    
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c&:(eq ?c execute)))
    ?ung <- (cell_to_unguess ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y))
=>
    (modify ?cell (status none))
    (printout t crlf)
    (printout t "I decide to do UNGUESS on (" ?x  ", " ?y ")"  crlf)
    (modify ?status (currently observe))
    (retract ?ung)
    (assert (exec (step ?s) (action unguess) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)
)

; se la cella è già guess. L'agente fa unguess poi fire 
(defrule do_fire_on_guess

    (moves (fires ?f&:(> ?f 0)))
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c&:(eq ?c execute)))
    ?fire <- (cell_to_fire ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?cf) (status guessed))
=>
    (modify ?cell (status fired))
    (printout t "I'm not very sure that in (" ?x  ", " ?y ") there is a piece of boat!"  crlf)
    (printout t "I decide to UNGUESS on (" ?x  ", " ?y ")"  crlf)
    (printout t "I decide to do a FIRE!" crlf)
    
    (modify ?status (currently observe))
    (retract ?fire)
    (assert (exec (step ?s) (action unguess) (x ?x) (y ?y)))
    (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)

)

; esegue una fire
(defrule do_fire

    (moves (fires ?f&:(> ?f 0)))
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c&:(eq ?c execute)))
    ?fire <- (cell_to_fire ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?cf))
=>
    (modify ?cell (status fired))
    (printout t "--------------- I decide to do a FIRE! ---------------" crlf)
    (printout t "With an evidence of " ?cf " I try to fire on (" ?x  ", " ?y ")" crlf)
    ;(printout t "------------------------------------------ CONTROL TO ENV " crlf)
    (modify ?status (currently observe))
    (retract ?fire)
    (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)

)

; esegue una guess
(defrule do_guess

    (moves (guesses ?g&:(> ?g 0)))
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c&:(eq ?c execute)))
    ?guess <- (cell_to_guess ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?cf))

=>
    (modify ?cell (status guessed))
    (printout t crlf)
    (printout t "I decide to do a GUESS:" crlf)
    (printout t "With an evidence of " ?cf ", I guess on cell: (" ?x  ", " ?y ")" crlf)
    ;(printout t "------------------------------------------ CONTROL TO ENV " crlf)
    (modify ?status (currently observe))
    (retract ?guess)
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)
)

; regola per terminare il gioco
; restituisce le evidenze sulle navi
(defrule do_solve

    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c&:(eq ?c execute)))
    ?solve <- (do_solve)
    
    (boat_number_type (type boat_1) (number ?n1))
    (boat_number_type (type boat_2) (number ?n2))
    (boat_number_type (type boat_3) (number ?n3))
    (boat_number_type (type boat_4) (number ?n4))

=>

    (printout t crlf)
    (printout t "Before the game is solved: I know something: " crlf)
    (printout t "Number of boat_1 supposed left before solve the game: " ?n1 crlf)
    (printout t "Number of boat_2 supposed left before solve the game: " ?n2 crlf)
    (printout t "Number of boat_3 supposed left before solve the game: " ?n3 crlf)
    (printout t "Number of boat_4 supposed left before solve the game: " ?n4 crlf)
    (printout t crlf)
    (printout t "------------------------------------------ I decide to SOLVE the game:" crlf)

    (modify ?status (currently finish))
    (retract ?solve)
    (assert (exec (step ?s) (action solve)))
    (pop-focus)
    (pop-focus)
)

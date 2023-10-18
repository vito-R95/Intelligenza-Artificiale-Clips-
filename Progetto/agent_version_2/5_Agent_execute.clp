;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT_EXECUTE (import AGENT_VIEW_PLAN ?ALL) (export ?ALL))




; esegue una fire
(defrule do_fire

    (moves (fires ?f&:(> ?f 0)))
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c))
    ?fire <- (cell_to_fire ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?cf))
=>
    (modify ?cell (status fired))
    (printout t crlf)
    (printout t "FIRE!" crlf)
    (printout t "With an evidence of " ?cf " I try to fire on (" ?x  ", " ?y ")" crlf)
    ;(printout t "------------------------------------------ CONTROL TO ENV " crlf)
    (modify ?status (currently plan))
    (retract ?fire)
    (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)

)

; esegue una guess
(defrule do_guess

    (moves (guesses ?g&:(> ?g 0)))
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c))
    ?guess <- (cell_to_guess ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?cf))

=>
    (modify ?cell (status guessed))
    (printout t crlf)
    (printout t "GUESS:" crlf)
    (printout t "With an evidence of " ?cf ", I guess on cell: (" ?x  ", " ?y ")" crlf)
    ;(printout t "------------------------------------------ CONTROL TO ENV " crlf)
    (modify ?status (currently plan))
    (retract ?guess)
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
    (pop-focus)
    (pop-focus)
)

; esegue una solve e termina il gioco
(defrule do_solve
    (status (step ?s)(currently running))
    ?status <- (agent_status (currently ?c))
    ?solve <- (do_solve)
=>
    (printout t crlf)
    (printout t "SOLVE:" crlf)
    (modify ?status (currently finish))
    (retract ?solve)
    (printout t "------------------------------------------ I finished to play!" crlf)
    (assert (exec (step ?s) (action solve)))
    (pop-focus)
    (pop-focus)
)

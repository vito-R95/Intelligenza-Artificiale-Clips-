;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT_PLAN (import AGENT_OBSERVE ?ALL) (export ?ALL))



; #############################################################
; ######### REGOLE PER DECIDERE CHE MOSSA FARE ################
; #############################################################



; regola per aggiornare i guess_CF nella rappresentazione della scacchiera dell'agente
; a seguito di un ricalcolo dovuto ad alcune evidenze 
; modifico sono i guess CF che non sono stati oggetto di inferenze (guess_CF < 50)

(defrule update_guess_CF_on_row_column (declare (salience 30))
    ?up <- (update_CF_on_cells ?x ?y)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(< ?g 50)))
    (not (k-cell (x ?x) (y ?y)))
    (k-per-row (row ?x) (num ?n_row))
    (k-per-col (col ?y) (num ?n_col))
=>
    (bind ?new (* ?n_row ?n_col))
    (modify ?cell (guess_CF ?new))
    (retract ?up)
)


; regola per stampare alcune modifiche ai guess cf
(defrule update_agent_cell (declare (salience 25))
    
    ?fact <- (agent_cell_to_update ?x ?y ?value)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g ?value)))
=>
    (modify ?cell (guess_CF ?value))
    (retract ?fact)
    (printout t crlf)
    (printout t "I update my knowledge on ["?x "," ?y "]" crlf)
    (printout t "I have an evidence of " ?value " that the boat is in coordinates: (" ?x  ", " ?y ")" crlf)
)



; L'agente ripensa alle guess fatte su alcune celle
(defrule unguess_on_certain_cells (declare (salience 5)) 
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    ?ung <- (unguess_on_cell ?x ?y)
    ?cell1 <- (agent_cell (x ?x) (y ?y) (status ?stat&:(eq ?stat guessed)))
=>
    (retract ?ung)
    (assert (cell_to_unguess ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)


; con questa regola, l'agente fa guess sulle posizioni inferite
(defrule guess_on_certainty_cells (declare (salience 5))
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    (moves (guesses ?ng&:(> ?ng 0)))
    ?g <- (guess_on_cell ?x ?y)
    ?cell1 <- (agent_cell (x ?x) (y ?y) (status ?stat&:(eq ?stat none)))
    
=>   
    (retract ?g)
    (assert (cell_to_guess ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)


; con questa regola, l'agente recupera la cella GUESSED con guess_CF alto 
; ma non pari a 100 tale che non esiste un'altra cella con guess_CF 
; di valore più alto che non sia 100.
; Successivamente sceglie di fare fire su quella cella
; questa regola serve per fare fire su una cella guess

(defrule find_cell_to_fire_if_agent_reconsider_move 
    (moves (fires ?f&:(> ?f 0)))
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    ?cell1 <- (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat guessed)))
    (test (neq ?value1 100))
    (not 
        (and
            (agent_cell (status ?stat) (guess_CF ?value2&:(neq ?value2 100)))
            (test(> ?value2 ?value1))
        )
    )

=>
    (assert (cell_to_fire ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)


; con questa regola, l'agente recupera la cella con guess_CF alto 
; ma non pari a 100 tale che non esiste un'altra cella con guess_CF 
; di valore più alto che non sia 100.
; Successivamente sceglie di fare fire su quella cella

(defrule find_cell_to_fire 
    (moves (fires ?f&:(> ?f 0)))
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    ?cell1 <- (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat none)))
    (test (neq ?value1 100))
    (not 
        (and
            (agent_cell (status ?stat) (guess_CF ?value2&:(neq ?value2 100)))
            (test(> ?value2 ?value1))
        )
    )

=>
    (assert (cell_to_fire ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)


; decide di fare guess se le fire sono finite
(defrule guess_if_fire_0
    (moves (fires 0) (guesses ?ng&:(> ?ng 0)))
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    
    ?cell1 <- (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat none)))
    (not (agent_cell (status ?stat) (guess_CF ?value2&:(> ?value2 ?value1))))
=>
    (assert (cell_to_guess ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)



; regola per terminare il gioco
; Il gioco termina se non ci sono più mosse a disposizione
(defrule decide_when_solve
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    
    (or
        (moves (fires 0) (guesses 0))
        (not (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat none))))
    )
=>
    (modify ?a_status (currently execute))
    (assert (do_solve))
    (pop-focus)
)


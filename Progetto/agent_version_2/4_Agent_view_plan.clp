;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT_VIEW_PLAN (import AGENT ?ALL) (export ?ALL))



; GESTIONE DEI NUMERI DI RIGHE E COLONNE  ----------------------------------
; questa regola consente di aggiornare il numero di pezzi di nave nella 
; riga e nella colonna in corrispondenza di un pezzo noto k-cell
; Modificando questi numeri permette di ricalcolare le evidenze guess_CF

(defrule update_col_row (declare (salience 40))
    ?update <- (update_row_col ?x ?y)
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
    ?fact1 <- (k-per-col (col ?y) (num ?n_row&:(> ?n_row 0))) 
    ?fact2 <- (k-per-row (row ?x) (num ?n_col&:(> ?n_col 0)))
=>
    (bind ?new_n_row (- ?n_row 1))
    (bind ?new_n_col (- ?n_col 1))
    (modify ?fact1 (num ?new_n_row))
    (modify ?fact2 (num ?new_n_col))
    (retract ?update)

    (assert (update_CF_on_cells 0 ?y))
    (assert (update_CF_on_cells 1 ?y))
    (assert (update_CF_on_cells 2 ?y))
    (assert (update_CF_on_cells 3 ?y))
    (assert (update_CF_on_cells 4 ?y))
    (assert (update_CF_on_cells 5 ?y))
    (assert (update_CF_on_cells 6 ?y))
    (assert (update_CF_on_cells 7 ?y))
    (assert (update_CF_on_cells 8 ?y))
    (assert (update_CF_on_cells 9 ?y))

    (assert (update_CF_on_cells ?x 0))
    (assert (update_CF_on_cells ?x 1))
    (assert (update_CF_on_cells ?x 2))
    (assert (update_CF_on_cells ?x 3))
    (assert (update_CF_on_cells ?x 4))
    (assert (update_CF_on_cells ?x 5))
    (assert (update_CF_on_cells ?x 6))
    (assert (update_CF_on_cells ?x 7))
    (assert (update_CF_on_cells ?x 8))
    (assert (update_CF_on_cells ?x 9))

    (printout t crlf)
    (printout t "################################################" crlf)
    (printout t "I reduce the number of hidden boats on: [row "?x ", col " ?y "]" crlf)
    (printout t "################################################" crlf)
)



; regola per aggiornare i guess_CF nella rappresentazione della scacchiera dell'agente
; a seguito di un ricalcolo dovuto ad alcune evidenze 
; modifico sono i guess CF che non sono stati oggetto di inferenze (guess_CF < 50)

(defrule update_guess_CF_on_row_column (declare (salience 35))
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



; ###########################################################################
; ####### REGOLE DI INFERENZA PER LE CELLE SCOPERTE CON LE FIRE #############
; ###########################################################################


; Regola per fare guess se so di una k-cell left
(defrule update_guess_CF_if_k_cell_left (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content left))
=>
    (bind ?new_y_value (+ ?y 1))
    (assert (agent_cell_to_update ?x ?new_y_value 100))
)



; Regola per fare guess se so di una k-cell right
(defrule update_guess_CF_if_k_cell_right (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content right))
=> 
    (bind ?new_y_value (- ?y 1))
    (assert (agent_cell_to_update ?x ?new_y_value 100))
)


; Regola per fare guess se so di una k-cell top
(defrule update_guess_CF_if_k_cell_up (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content top))
=> 
    (bind ?new_x_value (- ?x 1))
    (assert (agent_cell_to_update ?new_x_value ?y 100))
)


; Regola per fare guess se so di una k-cell bottom
(defrule update_guess_CF_if_k_cell_bot (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content bot))
=> 
    (bind ?new_x_value (+ ?x 1))
    (assert (agent_cell_to_update ?new_x_value ?y 100))
)



; GESTIONE DEI MIDDLE  -------------------------------------------------------------

; gestione di un middle sui bordi superiore ed inferiore
; se l'agente sa di un k-cell middle e questo è posto al bordo superiore o inferiore
; allora sa che la nave ha un orientamento orizzontale e ha almeno 3 pezzi

(defrule update_guess_CF_if_k_cell_middle_border_X (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_on_middle ?x ?y))
    (or 
        (test (eq (- ?x 1) -1))
        (test (eq (+ ?x 1) 10))
    )
=> 
    (bind ?y_left (- ?y 1))
    (bind ?y_right (+ ?y 1))
    (assert (agent_cell_to_update ?x ?y_left 100))
    (assert (agent_cell_to_update ?x ?y_right 100))
    (assert (agent_thought_on_middle ?x ?y))
)


; gestione di un middle sui bordi laterali destro e sinistro
; se l'agente sa di un k-cell middle e questo è posto al bordo laterale 
; allora sa che la nave ha un orientamento verticale e ha almeno 3 pezzi

(defrule update_guess_CF_if_k_cell_middle_border_Y (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_on_middle ?x ?y))

    (or 
        (test (eq (- ?y 1) -1))
        (test (eq (+ ?y 1) 10))
    )
=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (assert (agent_cell_to_update ?x_top ?y 100))
    (assert (agent_cell_to_update ?x_bottom ?y 100))
    (assert (agent_thought_on_middle ?x ?y))
)



; Se c'è un water a sinistra o a destra di un middle allora la nave è in verticale
(defrule update_guess_CF_if_water_middle_hor (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_again_on_middle ?x ?y))
    (or
        (k-cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 1))) (content water))
        (k-cell (x ?x) (y ?ny&:(eq ?ny (- ?y 1))) (content water))
    )
=>
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))
    (assert (agent_cell_to_update ?x_top ?y 100))
    (assert (agent_cell_to_update ?x_bottom ?y 100))
    (assert(agent_cell_to_update ?x ?pos_left 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))
    (assert (agent_thought_again_on_middle ?x ?y))
)

; Se c'è un water sopra o sotto di un middle allora la nave è in orizzontale
(defrule update_guess_CF_if_water_middle_ver (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_again_on_middle ?x ?y))
    (or
        (k-cell (x ?nx&:(eq ?nx (+ ?x 1))) (y ?y) (content water))
        (k-cell (x ?nx&:(eq ?nx (- ?x 1))) (y ?y) (content water))
    )
=>
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))
    (assert (agent_cell_to_update ?x_top ?y 0))
    (assert (agent_cell_to_update ?x_bottom ?y 0))
    (assert (agent_cell_to_update ?x ?pos_left 100))
    (assert (agent_cell_to_update ?x ?pos_right 100))
    (assert (agent_thought_again_on_middle ?x ?y))
)


; GESTIONE DEI MIDDLE CON NUMERI DI RIGHE E COLONNE  -------------------
; L'agente prova ad inferire l'orientamento della nave, se sa di un middle,
; a partire dal numero di pezzi di nave sulle righe e sulle colonne 

; Se numero_righe > numero_colonne aggiorna i CF a 60 sui 2 lati in orizzontale
(defrule update_guess_CF_if_k_cell_middle_hor (declare (salience 25))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_on_middle ?x ?y))

    (k-per-col (col ?y) (num ?n_row))
    (k-per-row (row ?x) (num ?n_col))
    (test (> ?n_row ?n_col))
=> 
    (bind ?y_left (- ?y 1))
    (bind ?y_right (+ ?y 1))
    (assert (agent_cell_to_update ?x ?y_left 60))
    (assert (agent_cell_to_update ?x ?y_right 60))
    (assert (agent_thought_on_middle ?x ?y))
)

; Se numero_righe < numero_colonne aggiorna i CF a 60 sui 2 lati in verticale
(defrule update_guess_CF_if_k_cell_middle_col (declare (salience 25))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_on_middle ?x ?y))

    (k-per-col (col ?y) (num ?n_row))
    (k-per-row (row ?x) (num ?n_col))
    (test (< ?n_row ?n_col))
=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (assert (agent_cell_to_update ?x_top ?y 60))
    (assert (agent_cell_to_update ?x_bottom ?y 60))
    (assert (agent_thought_on_middle ?x ?y))
)



; Se num_righe = num_colonne pone 4 guess, una su ogni lato (nord sud est ovest)
(defrule update_guess_CF_if_k_cell_middle (declare (salience 25))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (agent_thought_on_middle ?x ?y))
    
    (k-per-col (col ?y) (num ?n_row)) 
    (k-per-row (row ?x) (num ?n_col))
    (test (eq ?n_row ?n_col))
=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?y_left (- ?y 1))
    (bind ?y_right (+ ?y 1))
    (assert (agent_cell_to_update ?x_top ?y 50))
    (assert (agent_cell_to_update ?x_bottom ?y 50))
    (assert (agent_cell_to_update ?x ?y_left 50))
    (assert (agent_cell_to_update ?x ?y_right 50))
    (assert (agent_thought_on_middle ?x ?y))
)





; ###################################################################################
; ################## REGOLE PER AGGIORNARE I BLOCCHI INTORNO AD UNA FIRE ############
; ###################################################################################


; aggiorna le evidenze guess_CF attorno ad un k-cell left
(defrule update_position_near_boats_left (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content left))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up down left
    (assert(agent_cell_to_update ?pos_up ?y 0))
    (assert(agent_cell_to_update ?pos_down ?y 0))
    (assert(agent_cell_to_update ?x ?pos_left 0))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)

; aggiorna le evidenze guess_CF attorno ad un k-cell sub
(defrule update_position_near_boats_sub (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content sub))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up down left
    (assert(agent_cell_to_update ?pos_up ?y 0))
    (assert(agent_cell_to_update ?pos_down ?y 0))
    (assert(agent_cell_to_update ?x ?pos_left 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)

; aggiorna le evidenze guess_CF attorno ad un k-cell right
(defrule update_position_near_boats_right (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content right))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up down left
    (assert(agent_cell_to_update ?pos_up ?y 0))
    (assert(agent_cell_to_update ?pos_down ?y 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)


; aggiorna le evidenze guess_CF attorno ad un k-cell top
(defrule update_position_near_boats_top (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content top))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up down left
    (assert(agent_cell_to_update ?pos_up ?y 0))
    (assert(agent_cell_to_update ?x ?pos_left 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)

; aggiorna le evidenze guess_CF attorno ad un k-cell bottom
(defrule update_position_near_boats_bottom (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content bot))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up down left
    (assert(agent_cell_to_update ?pos_down ?y 0))
    (assert(agent_cell_to_update ?x ?pos_left 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)


; aggiorna le evidenze guess_CF attorno ad un k-cell middle
(defrule update_position_near_boats_middle (declare (salience 25))
    
    (k-cell (x ?x) (y ?y) (content middle))
=>
    (bind ?pos_up (- ?x 1))
    (bind ?pos_down (+ ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; modify up left, down left, up right, down right
    (assert(agent_cell_to_update ?pos_up ?pos_left 0))
    (assert(agent_cell_to_update ?pos_up ?pos_right 0))
    (assert(agent_cell_to_update ?pos_down ?pos_left 0))
    (assert(agent_cell_to_update ?pos_down ?pos_right 0))
)


; Aggiorna effettivamente ogni guess_cf sollevato in precedenza
(defrule update_agent_cell (declare (salience 20))
    
    ?fact <- (agent_cell_to_update ?x ?y ?value)
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g ?value)))
=>
    (modify ?cell (guess_CF ?value))
    (retract ?fact)
    (printout t crlf)
    (printout t "I update my knowledge on ["?x "," ?y "]" crlf)
    (printout t "I have an evidence of " ?value " that the boat is in coordinates: (" ?x  ", " ?y ")" crlf)
)





; #############################################################
; ######### REGOLE PER DECIDERE CHE MOSSA FARE ################
; #############################################################



; Se non ha più fire l'agente fa guess sulle celle con guess_cf più alto
(defrule find_cell_to_guess 
    (moves (fires 0) (guesses ?ng&:(> ?ng 0)))
    ?a_status <- (agent_status (currently ?c&:(eq ?c plan)))
    
    ?cell1 <- (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat none)))
    (not (agent_cell (status ?stat) (guess_CF ?value2&:(> ?value2 ?value1))))
=>
    (assert (cell_to_guess ?x ?y))
    (modify ?a_status (currently execute))
    (pop-focus)
)

; se ha ancora fire a disposizione, l'agente fa FIRE sulle celle con guess_cf più alto
; escludendo le certezze con guess_cf pari a 100
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



; Se l'agente non ha più mosse allora fa una solve
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

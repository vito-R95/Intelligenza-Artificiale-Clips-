;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT_OBSERVE (import AGENT ?ALL) (export ?ALL))


; Questo template riassume la mappa di gioco dal punto di vista dell'agente.
; L'agente riconosce una cella dalle coordinate X e Y, da uno stato e da un 
; certainty factor "guess_CF" che riassume la credenza dell'agente circa 
; la presenza o meno di un pezzo di nave in quella cella.
; Ogni cella può essere:
;   none: se non è stata ne osservata con una fire, ne guess
;   guessed: se è stata fatta una guess
;   fired:  se è stata osservata con una fire
(deftemplate agent_cell
    (slot x)
    (slot y)
    (slot status (allowed-values none guessed fired) (default none))
    (slot guess_CF (default -1)) 
)

; template per rappresentare il numero di navi che l'agente conosce inizialmente
(deftemplate boat_number_type
    (slot type (allowed-values boat_1 boat_2 boat_3 boat_4))
    (slot number)
)

; template che definisce le posizioni delle navi che l'agente prova ad inferire
; durante il gioco
(deftemplate boats_position
	(multislot xs)
	(multislot ys)
    (slot type (allowed-values boat_1 boat_2 boat_3 boat_4))
    (slot confident (allowed-values YES NO) (default NO))
)

; inizializzazione del numero di navi
(deffacts initial_facts
    (init_phase)
    (boat_number_type (type boat_1) (number 4))
    (boat_number_type (type boat_2) (number 3))
    (boat_number_type (type boat_3) (number 2))
    (boat_number_type (type boat_4) (number 1))
)


; Stampa info sulle celle che conosce all'inizio o dopo una fire
(defrule print-what-i-know-on-k-cell (declare (salience 40))
    (k-cell (x ?x) (y ?y) (content ?t) )
=>
	(printout t "I know that cell [" ?x ", " ?y "] contains " ?t "." crlf)
)


; Stampa info sul numero di pezzi di nave per ogni colonna
(defrule print-what-i-know-on-k-per-col (declare (salience 35))
    (k-per-col (col ?c) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that column " ?c " contains " ?n " pieces of boat. "  crlf)
)

; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-k-per-row (declare (salience 35))
    (k-per-row (row ?r) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that row " ?r " contains " ?n " pieces of boat. "  crlf)
)


; regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell (declare (salience 30))
    (init_phase)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g 100)))
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
=>
    (modify ?cell (guess_CF 100) (status fired))
)


; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-CF (declare (salience 30))
    (init_phase)
    (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(> ?g 0)))
=>
	(printout t "I have an initial guess_CF evidence of " ?g " that the boat is in coordinates: (" ?x  ", " ?y ")"  crlf)
)


; regola per aggiornare i guess_CF nella rappresentazione della scacchiera dell'agente
(defrule update_guess_CF (declare (salience 25))
    (init_phase)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(eq ?g -1)))
    (not (k-cell (x ?x) (y ?y)))
    (k-per-row (row ?x) (num ?n_row))
    (k-per-col (col ?y) (num ?n_col))
=>
    (bind ?new (* ?n_row ?n_col))
    (modify ?cell (guess_CF ?new))
)


; regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell (declare (salience 20))

    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g 100)))
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
=>
    (modify ?cell (guess_CF 100) (status fired))
    (assert (update_row_col ?x ?y))
)


; regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell_water (declare (salience 20))
    (init_phase)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g))
    (k-cell (x ?x) (y ?y) (content ?c&:(eq ?c water)))
=>
    (modify ?cell (guess_CF 0) (status fired))
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
)


; Conclude il set di print iniziali
(defrule terminate_init_phase (declare (salience 15))
    ?i <- (init_phase)
    ?status <- (agent_status (currently ?c))
=>
    (retract ?i)
    (modify ?status (currently plan))
)


; ---------------- Aggiorna il numero di navi che inferisce 

; aggiorna il numero di navi 
(defrule update_boat_number (declare (salience 10))
    ?fact <- (reduce_number_boat ?t)
    ?boat <- (boat_number_type (type ?t) (number ?value&:(> ?value 0)))
=>
    (bind ?new_value (- ?value 1))
    (modify ?boat (number ?new_value))
    (retract ?fact)
    (printout t crlf)
    (printout t "OBSERVE: " crlf)
    (printout t "I suppose there are only " ?new_value " more " ?t " left!" crlf)
)




; GESTIONE DEI NUMERI DI RIGHE E COLONNE  ----------------------------------
; questa regola consente di aggiornare il numero di pezzi di nave nella 
; riga e nella colonna in corrispondenza di un pezzo noto k-cell
; Modificando questi numeri permette di ricalcolare le evidenze guess_CF

(defrule update_col_row (declare (salience 10))
    ?update <- (update_row_col ?x ?y)
    (or
        (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
        (agent_cell (x ?x) (y ?y) (guess_CF ?cf&:(eq ?cf 100)))
    )
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
    (printout t "OBSERVE:" crlf)
    (printout t "I reduce the number of hidden boats on: [row "?x ", col " ?y "]" crlf)
)



; ##########################################################################
; ################## INDIVIDUAZIONE CERTA DELLE NAVI DA 1   ################
; ##########################################################################

; scopro di una nave da 1 se faccio fire su di lei
(defrule discover_boat_sub (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content sub))
=>
    (assert (boats_position (xs ?x) (ys ?y) (type boat_1) (confident YES)))
    (assert (reduce_number_boat boat_1))
)



; ##########################################################################
; ################## INDIVIDUAZIONE CERTA DELLE NAVI DA 2   ################
; ##########################################################################


; se trovo un left ad una cella dal bordo destro
(defrule discover_boat_2_left (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content left))
    (not (agent_cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 2)))))
=>
    (bind ?right (+ ?y 1))
    (assert (boats_position (xs ?x) (ys ?y ?right) (type boat_2) (confident YES)))
    (assert (reduce_number_boat boat_2))
)

; se trovo un right ad una cella dal bordo sinistro
(defrule discover_boat_2_right (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content right))
    (not (agent_cell (x ?x) (y ?ny&:(eq ?ny (- ?y 2)))))
=>
    (bind ?left (- ?y 1))
    (assert (boats_position (xs ?x) (ys ?left ?y) (type boat_2) (confident YES)))
    (assert (reduce_number_boat boat_2))
)


; se trovo un up ad una cella dal bordo inferiore
(defrule discover_boat_2_up (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content top))
    (not (agent_cell (x ?nx&:(eq ?nx (+ ?x 2))) (y ?y) ))
=>
    (bind ?down (+ ?x 1))
    (assert (boats_position (xs ?x ?down) (ys ?y) (type boat_2) (confident YES)))
    (assert (reduce_number_boat boat_2))
)


; se trovo un down ad una cella dal bordo superiore
(defrule discover_boat_2_down (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content bot))
    (not (agent_cell (x ?nx&:(eq ?nx (- ?x 2))) (y ?y) ))
=>
    (bind ?down (- ?x 1))
    (assert (boats_position (xs ?x ?down) (ys ?y) (type boat_2) (confident YES)))
    (assert (reduce_number_boat boat_2))
)



; ##########################################################################
; ################## INDIVIDUAZIONE CERTA DELLE NAVI DA 3   ################
; ##########################################################################


; se trovo un left e so che c'è un right a distanza di 2
(defrule discover_boat_3_if_left_right (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content left))
    (k-cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 2))) (content right))
=>
    (bind ?middle (+ ?y 1))
    (bind ?right (+ ?y 2))
    (assert (boats_position (xs ?x) (ys ?y ?middle ?right) (type boat_3) (confident YES)))
    (assert (reduce_number_boat boat_3))
    (assert (do_not_reconsider_on_k_cell ?x ?middle))
)


; se trovo un right e so che c'è un left a distanza di 2
(defrule discover_boat_3_if_right_left (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content right))
    (k-cell (x ?x) (y ?ny&:(eq ?ny (- ?y 2))) (content left))
=>
    (bind ?left (- ?y 2))
    (bind ?middle (- ?y 1))
    (assert (boats_position (xs ?x) (ys ?left ?middle ?y) (type boat_3) (confident YES)))
    (assert (reduce_number_boat boat_3))
    (assert (do_not_reconsider_on_k_cell ?x ?middle))
)

; se trovo un top e so che c'è un bot a distanza di 2
(defrule discover_boat_3_if_up_down (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content top))
    (k-cell (x ?nx&:(eq ?nx (- ?x 2))) (y ?y) (content bot))
=>
    (bind ?middle (- ?x 1))
    (bind ?bot (- ?x 2))
    (assert (boats_position (xs ?x ?middle ?bot) (ys y) (type boat_3) (confident YES)))
    (assert (reduce_number_boat boat_3))
    (assert (do_not_reconsider_on_k_cell ?middle ?y))
)

; se trovo un bot e so che c'è un top a distanza di 2
(defrule discover_boat_3_if_down_up (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content bot))
    (k-cell (x ?nx&:(eq ?nx (+ ?x 2))) (y ?y) (content top))
=>
    (bind ?top (+ ?x 2))
    (bind ?middle (+ ?x 1))
    (assert (boats_position (xs ?top ?middle ?y) (ys y) (type boat_3) (confident YES)))
    (assert (reduce_number_boat boat_3))
    (assert (do_not_reconsider_on_k_cell ?middle ?y))
)


; ##########################################################################
; ################## INDIVIDUAZIONE CERTA DELLE NAVI DA 4   ################
; ##########################################################################


; se trovo un middle e so che c'è un middle in alto a distanza di 1
(defrule discover_boat_4_ver_type1 (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-cell (x ?nx&:(eq ?nx (+ ?x 1))) (y ?y) (content middle))
    (not(do_not_reconsider_on_k_cell ?x ?y))
=>
    (bind ?top (+ ?x 2))
    (bind ?middle1 (+ ?x 1))
    (bind ?bot (- ?x 1))

    (assert (boats_position (xs ?top ?y ?middle1 ?bot) (ys y) (type boat_4) (confident YES)))
    (assert (reduce_number_boat boat_4))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
    (assert (do_not_reconsider_on_k_cell ?middle1 ?y))

    (assert (agent_cell_to_update ?top ?y 100))
    (assert (agent_cell_to_update ?bot ?y 100))
    
    (assert (guess_on_cell ?top ?y))
    (assert (guess_on_cell ?bot ?y))
    
    (assert (update_row_col ?top ?y))
    (assert (update_row_col ?bot ?y))
)

; se trovo un middle in alto e so che c'è un middle in basso a distanza di 1
(defrule discover_boat_4_ver_type2 (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-cell (x ?nx&:(eq ?nx (- ?x 1))) (y ?y) (content middle))
    (not(do_not_reconsider_on_k_cell ?x ?y))
=>
    (bind ?top (- ?x 2))
    (bind ?middle1 (- ?x 1))
    (bind ?bot (+ ?x 1))

    (assert (boats_position (xs ?top ?y ?middle1 ?bot) (ys y) (type boat_4) (confident YES)))
    (assert (reduce_number_boat boat_4))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
    (assert (do_not_reconsider_on_k_cell ?middle1 ?y))

    (assert (agent_cell_to_update ?top ?y 100))
    (assert (agent_cell_to_update ?bot ?y 100))
    
    (assert (guess_on_cell ?top ?y))
    (assert (guess_on_cell ?bot ?y))
    
    (assert (update_row_col ?top ?y))
    (assert (update_row_col ?bot ?y))
)

; se trovo un middle a sinistra e so che c'è un middle a destra a distanza di 1
(defrule discover_boat_4_hor_type1 (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 1))) (content middle))
    (not(do_not_reconsider_on_k_cell ?x ?y))
=>
    (bind ?left (- ?y 1))
    (bind ?middle1 (+ ?y 1))
    (bind ?right (+ ?y 2))

    (assert (boats_position (xs x) (ys ?left ?middle1 ?y ?right) (type boat_4) (confident YES)))
    (assert (reduce_number_boat boat_4))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
    (assert (do_not_reconsider_on_k_cell ?x ?middle1))
    
    (assert (agent_cell_to_update ?x ?left 100))
    (assert (agent_cell_to_update ?x ?right 100))
    
    (assert (guess_on_cell ?x ?left))
    (assert (guess_on_cell ?x ?right))
    
    (assert (update_row_col ?x ?left))
    (assert (update_row_col ?x ?right))

)

; se trovo un middle a destra e so che c'è un middle a sinistra a distanza di 1
(defrule discover_boat_4_hor_type2 (declare (salience 12))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-cell (x ?x) (y ?ny&:(eq ?ny (- ?y 1))) (content middle))
    (not(do_not_reconsider_on_k_cell ?x ?y))
=>
    (bind ?left (- ?y 2))
    (bind ?middle1 (- ?y 1))
    (bind ?right (+ ?y 1))

    (assert (boats_position (xs x) (ys ?left ?middle1 ?y ?right) (type boat_4) (confident YES)))
    (assert (reduce_number_boat boat_4))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
    (assert (do_not_reconsider_on_k_cell ?x ?middle1))

    (assert (agent_cell_to_update ?x ?left 100))
    (assert (agent_cell_to_update ?x ?right 100))
    
    (assert (guess_on_cell ?x ?left))
    (assert (guess_on_cell ?x ?right))
    
    (assert (update_row_col ?x ?left))
    (assert (update_row_col ?x ?right))
)



; ####################################################################
; ################## GESTIONE DEI PEZZI DI NAVE CERTI ################
; ####################################################################


; Regola per fare guess se so di una k-cell left
(defrule update_guess_CF_if_k_cell_left (declare (salience 9))
    (k-cell (x ?x) (y ?y) (content left))
=>
    (bind ?new_y_value (+ ?y 1))
    (assert (agent_cell_to_update ?x ?new_y_value 100))
    (assert (guess_on_cell ?x ?new_y_value))
    (assert (update_row_col ?x ?new_y_value))
    (assert (boats_position (xs ?x) (ys ?y ?new_y_value) (type boat_2) (confident NO)))
    (assert (reduce_number_boat boat_2))
)



; Regola per fare guess se so di una k-cell right
(defrule update_guess_CF_if_k_cell_right (declare (salience 9))
    (k-cell (x ?x) (y ?y) (content right))
=> 
    (bind ?new_y_value (- ?y 1))
    (assert (agent_cell_to_update ?x ?new_y_value 100))
    (assert (guess_on_cell ?x ?new_y_value))
    (assert (update_row_col ?x ?new_y_value))
    (assert (boats_position (xs ?x) (ys ?new_y_value ?y) (type boat_2) (confident NO)))
    (assert (reduce_number_boat boat_2))
)


; Regola per fare guess se so di una k-cell top
(defrule update_guess_CF_if_k_cell_up (declare (salience 9))
    (k-cell (x ?x) (y ?y) (content top))
=> 
    (bind ?new_x_value (+ ?x 1))
    (assert (agent_cell_to_update ?new_x_value ?y 100))
    (assert (guess_on_cell ?new_x_value ?y))
    (assert (update_row_col ?new_x_value ?y))
    (assert (boats_position (xs ?x ?new_x_value) (ys ?y) (type boat_2) (confident NO)))
    (assert (reduce_number_boat boat_2))
)



; Regola per fare guess se so di una k-cell bottom
(defrule update_guess_CF_if_k_cell_bot (declare (salience 9))
    (k-cell (x ?x) (y ?y) (content bot))
=> 
    (bind ?new_x_value (- ?x 1))
    (assert (agent_cell_to_update ?new_x_value ?y 100))
    (assert (guess_on_cell ?new_x_value ?y))
    (assert (update_row_col ?new_x_value ?y))
    (assert (boats_position (xs ?new_x_value ?x) (ys ?y) (type boat_2) (confident NO)))
    (assert (reduce_number_boat boat_2))
)




; ################################################################
; ################## GESTIONE DEI MIDDLE #########################
; ################################################################



; se so di un middle e non ci sono pezzi di nave nella celle laterali
; o becco un water a sinistra o a destra allora la nave è in verticale
; inoltre scopro di una nave almeno da 3

(defrule update_guess_CF_if_water_middle_hor (declare (salience 8))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-per-col (col ?c1&:(eq ?c1 (+ ?y 1))) (num ?n_c1))
    (k-per-col (col ?c2&:(eq ?c2 (- ?y 1))) (num ?n_c2))

    (not (do_not_reconsider_on_k_cell ?x ?y))
    (or
        (test (eq ?n_c1 0))
        (test (eq ?n_c2 0))
        (k-cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 1))) (content water))
        (k-cell (x ?x) (y ?ny&:(eq ?ny (- ?y 1))) (content water))
    )
=>
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ;boat type
    (assert (boats_position (xs ?x_top ?x ?x_bottom) (ys ?y) (type boat_3) (confident NO)))
    (assert (reduce_number_boat boat_3))

    ; unguess
    (assert (unguess_on_cell ?x ?pos_left))
    (assert (unguess_on_cell ?x ?pos_right))

    ; guess
    (assert (guess_on_cell ?x_top ?y))
    (assert (guess_on_cell ?x_bottom ?y))

    ; update GUESS CF
    (assert (agent_cell_to_update ?x_top ?y 100))
    (assert (agent_cell_to_update ?x_bottom ?y 100))
    (assert (update_row_col ?x_top ?y))
    (assert (update_row_col ?x_bottom ?y))

    (assert(agent_cell_to_update ?x ?pos_left 0))
    (assert(agent_cell_to_update ?x ?pos_right 0))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
)



; se so di un middle e non ci sono pezzi di nave nella cella superiore o inferiore
; o becco un water sopra o sotto allora la nave è in orizzontale
; inoltre scopro di una nave almeno da 3

(defrule update_guess_CF_if_water_middle_ver (declare (salience 8))
    (k-cell (x ?x) (y ?y) (content middle))
    (k-per-row (row ?r1&:(eq ?r1 (+ ?x 1))) (num ?n_r1))
    (k-per-row (row ?r2&:(eq ?r2 (- ?x 1))) (num ?n_r2))
    
    (not (do_not_reconsider_on_k_cell ?x ?y))
    (or
        (test (eq ?n_r1 0))
        (test (eq ?n_r2 0))
        (k-cell (x ?nx1&:(eq ?nx1 (+ ?x 1))) (y ?y) (content water))
        (k-cell (x ?nx2&:(eq ?nx2 (- ?x 1))) (y ?y) (content water))
    )
=>
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ;boat type
    (assert (boats_position (xs ?x) (ys ?pos_left ?y ?pos_right) (type boat_3) (confident NO)))
    (assert (reduce_number_boat boat_3))

    ;unguess
    (assert (unguess_on_cell ?x_top ?y))
    (assert (unguess_on_cell ?x_bottom ?y))

    ; guess
    (assert (guess_on_cell ?x ?pos_left))
    (assert (guess_on_cell ?x ?pos_right))

    ; update GUESS CF
    (assert (agent_cell_to_update ?x_top ?y 0))
    (assert (agent_cell_to_update ?x_bottom ?y 0))
    (assert (agent_cell_to_update ?x ?pos_left 100))
    (assert (agent_cell_to_update ?x ?pos_right 100))
    
    (assert (update_row_col ?x ?pos_left))
    (assert (update_row_col ?x ?pos_right))

    (assert (do_not_reconsider_on_k_cell ?x ?y))
)



; gestione di un middle sui bordi superiore ed inferiore
(defrule update_guess_CF_if_k_cell_middle_border_X (declare (salience 7))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (do_not_reconsider_on_k_cell ?x ?y))
    (or 
        (test (eq (- ?x 1) -1))
        (test (eq (+ ?x 1) 10))
    )
=> 
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    (assert (agent_cell_to_update ?x ?pos_left 100))
    (assert (agent_cell_to_update ?x ?pos_right 100))
    (assert (boats_position (xs ?x) (ys ?pos_left ?y ?pos_right) (type boat_3) (confident NO)))
    (assert (reduce_number_boat boat_3))

    (assert (update_row_col ?x ?pos_left))
    (assert (update_row_col ?x ?pos_right))

    (assert (guess_on_cell ?x ?pos_left))
    (assert (guess_on_cell ?x ?pos_right))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
)


; gestione di un middle sui bordi laterali destro e sinistro
(defrule update_guess_CF_if_k_cell_middle_border_Y (declare (salience 7))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (do_not_reconsider_on_k_cell ?x ?y))

    (or 
        (test (eq (- ?y 1) -1))
        (test (eq (+ ?y 1) 10))
    )
=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))

    (assert (agent_cell_to_update ?x_top ?y 100))
    (assert (agent_cell_to_update ?x_bottom ?y 100))
    (assert (boats_position (xs ?x_top ?x ?x_bottom) (ys ?y) (type boat_3) (confident NO)))
    (assert (reduce_number_boat boat_3))

    (assert (update_row_col ?x_top ?y))
    (assert (update_row_col ?x_bottom ?y))

    (assert (guess_on_cell ?x_top ?y))
    (assert (guess_on_cell ?x_bottom ?y))
    (assert (do_not_reconsider_on_k_cell ?x ?y))
)




; GESTIONE DEI MIDDLE CON NUMERI DI RIGHE E COLONNE  -------------------
; L'agente prova ad inferire l'orientamento della nave, se sa di un middle,
; a partire dal numero di pezzi di nave sulle righe e sulle colonne 
; La differenza con la versione 2 è che qui l'agente fa direttamente guess.
; se ritiene di aver sbagliato, farà una unguess


; Se numero_righe > numero_colonne aggiorna i CF a 60 sui 2 lati in orizzontale
(defrule update_guess_CF_if_k_cell_middle_hor (declare (salience 6))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (do_not_reconsider_on_k_cell ?x ?y))

    (k-per-col (col ?y) (num ?n_row))
    (k-per-row (row ?x) (num ?n_col))
    (test (> ?n_row ?n_col))
=> 
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    (assert (guess_on_cell ?x ?pos_left))
    (assert (guess_on_cell ?x ?pos_right))
    (assert (agent_cell_to_update ?x ?pos_left 60))
    (assert (agent_cell_to_update ?x ?pos_right 60))
)


; Se numero_righe < numero_colonne aggiorna i CF a 60 sui 2 lati in verticale
(defrule update_guess_CF_if_k_cell_middle_col (declare (salience 6))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (do_not_reconsider_on_k_cell ?x ?y))

    (k-per-col (col ?y) (num ?n_row))
    (k-per-row (row ?x) (num ?n_col))
    (test (< ?n_row ?n_col))
=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))


    (assert (guess_on_cell ?x_top ?y))
    (assert (guess_on_cell ?x_bottom ?y))
    (assert (agent_cell_to_update ?x_top ?y 60))
    (assert (agent_cell_to_update ?x_bottom ?y 60))

)


; Se num_righe = num_colonne pone 4 guess, una su ogni lato (nord sud est ovest)
(defrule update_guess_CF_if_k_cell_middle (declare (salience 6))
    (k-cell (x ?x) (y ?y) (content middle))
    (not (do_not_reconsider_on_k_cell ?x ?y))
    
    (k-per-col (col ?y) (num ?n_row)) 
    (k-per-row (row ?x) (num ?n_col))
    (test (eq ?n_row ?n_col))

=> 
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; guess
    (assert (guess_on_cell ?x_top ?y))
    (assert (guess_on_cell ?x_bottom ?y))
    (assert (guess_on_cell ?x ?pos_left))
    (assert (guess_on_cell ?x ?pos_right))

    (assert (agent_cell_to_update ?x_top ?y 50))
    (assert (agent_cell_to_update ?x_bottom ?y 50))
    (assert (agent_cell_to_update ?x ?pos_left 50))
    (assert (agent_cell_to_update ?x ?pos_right 50))
)



; Stampa tutte le inferenze sulle posizioni delle navi con incertezza
(defrule print-what-i-know-on-boats_position_no (declare (salience 5))
    (boats_position (xs $?x) (ys $?y) (type ?t) (confident ?c&:(eq ?c NO)))
=>
    (printout t "I'm not very sure, but I guess there is a " ?t " on coordinates [xs: " $?x ", ys: " $?y " ]" crlf)
)

; Stampa tutte le inferenze sulle posizioni delle navi con certezza
(defrule print-what-i-know-on-boats_position_yes (declare (salience 5))
    (boats_position (xs $?x) (ys $?y) (type ?t) (confident ?c&:(eq ?c YES)))
=>
    (printout t "I discover, a " ?t " on coordinates [xs: " $?x ", ys: " $?y " ]" crlf)
)





; ###################################################################################
; ################## REGOLE PER AGGIORNARE I BLOCCHI INTORNO AD UNA FIRE ############
; ###################################################################################


; aggiorna le evidenze guess_CF attorno ad un k-cell left
(defrule update_position_near_boats_left (declare (salience 5))
    
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
(defrule update_position_near_boats_sub (declare (salience 5))
    
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
(defrule update_position_near_boats_right (declare (salience 5))
    
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
(defrule update_position_near_boats_top (declare (salience 5))
    
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
(defrule update_position_near_boats_bottom (declare (salience 5))
    
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
(defrule update_position_near_boats_middle (declare (salience 5))
    
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





; Dopo aver osservato l'ambiente prepara il piano
(defrule pass_to_plan 
    (not (init_phase))
    ?status <- (agent_status (currently ?c))
=>
    (modify ?status (currently plan))
    (pop-focus)
)
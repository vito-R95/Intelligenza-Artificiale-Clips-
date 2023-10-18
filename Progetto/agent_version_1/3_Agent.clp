;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------

(defmodule AGENT (import MAIN ?ALL) (import ENV ?ALL) (export ?ALL))


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




; #######################################################################
; ################## REGOLE PER INIZIALIZZAZIONE ########################
; #######################################################################


; Stampa info sulle celle che conosce all'inizio o dopo una fire
(defrule print-what-i-know-on-k-cell (declare (salience 40))
    (k-cell (x ?x) (y ?y) (content ?t) )
=>
	(printout t "I know that cell [" ?x ", " ?y "] contains " ?t "." crlf)
)


; Stampa info sul numero di pezzi di nave per ogni colonna
(defrule print-what-i-know-on-k-per-col (declare (salience 35))
	(init)
    (k-per-col (col ?c) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that column " ?c " contains " ?n " pieces of boat. "  crlf)
)

; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-k-per-row (declare (salience 35))
	(init)
    (k-per-row (row ?r) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that row " ?r " contains " ?n " pieces of boat. "  crlf)
)



; Regola per aggiornare i guess_CF nella rappresentazione della scacchiera dell'agente
; ogni cella ha un guess_cf calcolato come il prodotto dei valori row e col associati 
; alle coordinate della cella, indicanti il numero di pezzi di nave sulla riga e sulla
; colonna della cella.
(defrule update_guess_CF (declare (salience 30))
    (init)
    (status (currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(eq ?g -1)))
    (not (k-cell (x ?x) (y ?y)))
    (k-per-row (row ?x) (num ?n_row))
    (k-per-col (col ?y) (num ?n_col))
=>
    (bind ?new (* ?n_row ?n_col))
    (modify ?cell (guess_CF ?new))
)


; Regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell (declare (salience 30))
    (init)
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g 100)))
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
=>
    (modify ?cell (guess_CF 100) (status fired))
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
)



; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-CF (declare (salience 30))
    (init)
    (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(> ?g 0)))
=>
	(printout t "I have an initial evidence of " ?g " that the boat is in coordinates: (" ?x  ", " ?y ")"  crlf)
)


; Conclude il set di info iniziali
(defrule stop_init_print (declare (salience 25))
    (init)
    ?i <- (init)
=>
    (retract ?i)
    (printout t "Init phase finished! " crlf crlf)
)



; ###########################################################################
; ####### REGOLE DI INFERENZA PER LE CELLE SCOPERTE CON LE FIRE #############
; ###########################################################################


; Regola per fare guess se so di una k-cell left
(defrule guess_left (declare (salience 20)) 
    
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    (k-cell (x ?x) (y ?y) (content left))
    ?cell <- (agent_cell (x ?x) (y ?ny&:(eq ?ny (+ ?y 1))) (guess_CF ?g&:(neq ?g 100))(status ?stat&:(eq ?stat none)))
=> 
    (bind ?new_y_value (+ ?y 1))
    
    (modify ?cell (status guessed))
    (printout t "As there is a left block in (" ?x  ", " ?y ") I decide to do a GUESS on cell: " crlf)
    (printout t "RIGHT: (" ?x  ", " ?new_y_value ")" crlf)
    (printout t crlf)
    (assert (exec (step ?s) (action guess) (x ?x) (y ?new_y_value)))
    (pop-focus)
)


; Regola per fare guess se so di una k-cell right
(defrule guess_right (declare (salience 20))
    
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    (k-cell (x ?x) (y ?y) (content right))
    ?cell <- (agent_cell (x ?x) (y ?ny&:(eq ?ny (- ?y 1))) (guess_CF ?g&:(neq ?g 100))(status ?stat&:(eq ?stat none)))
=> 
    (bind ?new_y_value (- ?y 1))
    (modify ?cell (status guessed))
    (printout t "As there is a right block in (" ?x  ", " ?y ") I decide to do a GUESS on cell: " crlf)
    (printout t "LEFT: (" ?x  ", " ?new_y_value ")" crlf)
    (printout t crlf)
    (assert (exec (step ?s) (action guess) (x ?x) (y ?new_y_value)))
    (pop-focus)
)


; Regola per fare guess se so di una k-cell top
(defrule guess_up (declare (salience 20))
    
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    (k-cell (x ?x) (y ?y) (content top))
    ?cell <- (agent_cell (x ?nx&:(eq ?nx (- ?x 1))) (y ?y) (guess_CF ?g&:(neq ?g 100))(status ?stat&:(eq ?stat none)))
=> 
    (bind ?new_x_value (- ?x 1))
    (modify ?cell (status guessed))
    (printout t "As there is a top block in (" ?x  ", " ?y ") I decide to do a GUESS on cell: " crlf)
    (printout t "BOTTOM: (" ?new_x_value  ", " ?y ")" crlf)
    (printout t crlf)

    (assert (exec (step ?s) (action guess) (x ?new_x_value) (y ?y)))
    (pop-focus)
)



; Regola per fare guess se so di una k-cell bottom
(defrule guess_bottom (declare (salience 20))
    
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    (k-cell (x ?x) (y ?y) (content bot))
    ?cell <- (agent_cell (x ?nx&:(eq ?nx (+ ?x 1))) (y ?y) (guess_CF ?g&:(neq ?g 100)) (status ?stat&:(eq ?stat none)) )
=> 
    (bind ?new_x_value (+ ?x 1))
    (modify ?cell (status guessed))
    (printout t "As there is a bottom block in (" ?x  ", " ?y ") I decide to do a GUESS on cell: " crlf)
    (printout t "TOP: (" ?new_x_value  ", " ?y ")" crlf)
    (printout t crlf)
    (assert (exec (step ?s) (action guess) (x ?new_x_value) (y ?y)))
    (pop-focus)
)



; Regola per fare guess se so di una k-cell middle
; In questo caso l'agente 1 pone 4 guess, una su ogni lato (nord sud est ovest) dato il middle. 
; L'agente ignora completamente l'orientamento della nave, ne prova ad inferirlo.

(defrule update_guess_CF_if_k_cell_middle (declare (salience 20))
    (status (step ?s)(currently running))
	(moves (guesses ?ng&:(>= ?ng 4)))
    (k-cell (x ?x) (y ?y) (content middle))
    ?cell <- (agent_cell (x ?nx&:(eq ?nx (+ ?x 1))) (y ?y) (status ?stat&:(eq ?stat none)))
=> 
    (modify ?cell (status guessed))
    (bind ?x_top (+ ?x 1))
    (bind ?x_bottom (- ?x 1))
    (bind ?pos_left (- ?y 1))
    (bind ?pos_right (+ ?y 1))

    ; guess
    (assert (exec (step ?s) (action guess) (x ?x_top) (y ?y)))
    (assert (exec (step ?s) (action guess) (x ?x_bottom) (y ?y)))
    (assert (exec (step ?s) (action guess) (x ?x) (y ?pos_left)))
    (assert (exec (step ?s) (action guess) (x ?x) (y ?pos_right)))

    (printout t "I don't know how is oriented the boat, so I guess on cells: " crlf)
    (printout t "NORD: (" ?x_top  ", " ?y ")," crlf)
    (printout t "SUD: (" ?x_bottom  ", " ?y ")," crlf)
    (printout t "EST: (" ?x  ", " ?pos_right ")," crlf)
    (printout t "OVEST: (" ?x  ", " ?pos_left ")" crlf)

)


; ###########################################################
; ################## REGOLE PER FARE GUESS ##################
; ###########################################################



; Regola per fare guess sulle celle restanti dopo aver fatto le fire, 
; aventi valore di guess_cf più alto. 
(defrule do_guess
    
	(status (step ?s)(currently running))
	(moves (guesses ?ng&:(> ?ng 0)))
    
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?value1&:(> ?value1 0)) (status ?stat&:(eq ?stat none)))
    (not (agent_cell (status ?stat) (guess_CF ?value2&:(> ?value2 ?value1))))

=>
    (modify ?cell (status guessed))
    (printout t crlf)
    (printout t "GUESS:" crlf)
    (printout t "With an evidence of " ?value1 ", I guess on cell: (" ?x  ", " ?y ")" crlf)
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
    (pop-focus)
)



; ##########################################################################
; ################## REGOLE PER TERMINARE LA PARTITA #######################
; ##########################################################################


; L'agente termina la partita solo quando a finito tutte le mosse fire 
; e guess a disposizione.
(defrule do_solve
    (status (step ?s)(currently running))
	(moves (guesses 0))
=>
    (printout t crlf)    
    (printout t "------------------------------------------ I finished to play!" crlf)
    (assert (exec (step ?s) (action solve)))
    (pop-focus)
)

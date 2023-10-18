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

; Questo template gestisce lo stato dell'agente e serve per alternare l'esecuzione 
; dei vari moduli di cui è composto l'agente

(deftemplate agent_status 
    (slot currently (allowed-values main plan execute finish)) 
)

; inizializzazione dello stato dell'agente
; il fatto init serve per eseguire alcune regole solo una volta
(deffacts initial_facts
    (init)
    (agent_status (currently main))
)


; #######################################################################
; ################## REGOLE PER INIZIALIZZAZIONE ########################
; #######################################################################


; Stampa info sulle celle che conosce all'inizio o dopo una fire
(defrule print-what-i-know-on-k-cell (declare (salience 30))
    (k-cell (x ?x) (y ?y) (content ?t) )
=>
	(printout t "I know that cell [" ?x ", " ?y "] contains " ?t "." crlf)
)


; Stampa info sul numero di pezzi di nave per ogni colonna
(defrule print-what-i-know-on-k-per-col (declare (salience 25))
    (k-per-col (col ?c) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that column " ?c " contains " ?n " pieces of boat. "  crlf)
)

; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-k-per-row (declare (salience 25))
    (k-per-row (row ?r) (num ?n&:(> ?n 0)))
=>
	(printout t "I know that row " ?r " contains " ?n " pieces of boat. "  crlf)
)



; regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell (declare (salience 20))
    (init)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g 100)))
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
=>
    (modify ?cell (guess_CF 100) (status fired))
)



; Stampa info sul numero di pezzi di nave per ogni riga
(defrule print-what-i-know-on-CF (declare (salience 20))
    (init)
    (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(> ?g 0)))
=>
	(printout t "I have an initial guess_CF evidence of " ?g " that the boat is in coordinates: (" ?x  ", " ?y ")"  crlf)
)


; regola per inizializzare i guess_CF nella rappresentazione della scacchiera dell'agente
(defrule update_guess_CF (declare (salience 15))
    (init)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(eq ?g -1)))
    (not (k-cell (x ?x) (y ?y)))
    (k-per-row (row ?x) (num ?n_row))
    (k-per-col (col ?y) (num ?n_col))
=>
    (bind ?new (* ?n_row ?n_col))
    (modify ?cell (guess_CF ?new))
)


; regola per inizializzare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell (declare (salience 10))

    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g&:(neq ?g 100)))
    (k-cell (x ?x) (y ?y) (content ?c&:(neq ?c water)))
=>
    (modify ?cell (guess_CF 100) (status fired))
    (assert (update_row_col ?x ?y))
    (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))    
)


; regola per aggiornare i guess_CF nel caso di celle già note
(defrule update_guess_CF_on_k_cell_water (declare (salience 10))
    (init)
    (status (step ?s)(currently running))
    ?cell <- (agent_cell (x ?x) (y ?y) (guess_CF ?g))
    (k-cell (x ?x) (y ?y) (content ?c&:(eq ?c water)))
=>
    (modify ?cell (guess_CF 0) (status fired))
)


; Conclude il set di info iniziali
(defrule go_on_plan_first (declare (salience 5))
    ?i <- (init)
    ?status <- (agent_status (currently main))
=>
    (retract ?i)
    (printout t "Init phase finished! " crlf crlf)
    (modify ?status (currently plan))
    ;(printout t "------------------------------------------ I'm planning" crlf)
    (focus AGENT_VIEW_PLAN)
)


; passa al modulo di deliberazione
(defrule go-on-execute (declare (salience 5))
   (agent_status (currently execute))
 =>
    ;(printout t "------------------------------------------ I'm executing an action " crlf)
    (focus AGENT_EXECUTE)

)

; passa al modulo di piafinicazione
(defrule go-on-plan  (declare (salience 5))
  (agent_status (currently plan))
=>
    ;(printout t "------------------------------------------ I'm planning" crlf)
    (focus AGENT_VIEW_PLAN)
)

; conclude l'esecuzione dell'agente
; esegue un pop-focus al Main
(defrule final_step
    (agent_status (currently finish))
    (status (step ?s)(currently running))
=>
    (assert (exec (step ?s) (action solve)))
    (pop-focus)
)

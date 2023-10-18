(defmodule MAIN (export ?ALL))

;;template che rappresenta la mossa eseguita al passo ?step
(deftemplate exec
   (slot step)
   (slot action (allowed-values fire guess unguess solve))
   (slot x) ;;non usato nel caso del comando solve
   (slot y) ;;non usato nel caso del comando solve
)

;;stato corrente dell'esecuzione
(deftemplate status (slot step) (slot currently (allowed-values running stopped)) )

;;numero di mosse ancora disponibili (tra fire e guess)
(deftemplate moves (slot fires) (slot guesses) )

(deftemplate statistics
	(slot num_fire_ok)
	(slot num_fire_ko)
	(slot num_guess_ok)
	(slot num_guess_ko)
	(slot num_safe)
	(slot num_sink)
)



(defrule go-on-env-first (declare (salience 30))
  ?f <- (first-pass-to-env)
=>

  (retract ?f)
  (focus ENV)
)


(defrule go-on-agent  (declare (salience 20))
   (maxduration ?d)
   (status (step ?s&:(< ?s ?d)) (currently running))

 =>

    ;(printout t crlf crlf)
    ;(printout t "vado ad agent  step" ?s)
    (focus AGENT)
)



; SI PASSA AL MODULO ENV DOPO CHE AGENTE HA DECISO AZIONE DA FARE

(defrule go-on-env  (declare (salience 30))
  ?f1<-	(status (step ?s))
  (exec (step ?s)) 	;// azione da eseguire al passo s, viene simulata dall'environment

=>

  ; (printout t crlf crlf)
  ; (printout t "vado ad ENV  step" ?s)
  (focus ENV)

)

(defrule game-over
	(maxduration ?d)
	(status (step ?s&:(>= ?s ?d)) (currently running))
=>
	(assert (exec (step ?s) (action solve)))
	(focus ENV)
)


; Ho aggiunto solo questa regola per avere la statistica finale aggiornata e stampata a video
(defrule final_print
  (status (step ?s) (currently stopped))
  (moves (fires ?f) (guesses ?g))
  (statistics (num_fire_ok ?fok) (num_fire_ko ?fko) (num_guess_ok ?gok) (num_guess_ko ?gko) (num_safe ?safe) (num_sink ?sink))
=>
  (printout t crlf)
  (printout t "Number of steps: " ?s crlf)
  (printout t "Number fire ok: " ?fok crlf)
  (printout t "Number fire ko: " ?fko crlf)
  (printout t "Number guess ok: " ?gok crlf)
  (printout t "Number guess ko: " ?gko crlf)
  (printout t "Number piece of boats safe: " ?safe crlf)
  (printout t "Number of boats destroyed: " ?sink crlf)
)

(deffacts initial-facts
	(maxduration 100)
	(status (step 0) (currently running))
  (statistics (num_fire_ok 0) (num_fire_ko 0) (num_guess_ok 0) (num_guess_ko 0) (num_safe 0) (num_sink 0))
	(first-pass-to-env)
	(moves (fires 5) (guesses 20) )
)


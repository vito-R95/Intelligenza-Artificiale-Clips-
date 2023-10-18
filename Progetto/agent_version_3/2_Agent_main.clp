;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import ENV ?ALL) (export ?ALL))


; Questo template gestisce lo stato dell'agente e serve per alternare l'esecuzione 
; dei vari moduli di cui è composto l'agente
(deftemplate agent_status 
    (slot currently (allowed-values main observe plan execute finish)) 
)

; inizializzazione dello stato dell'agente
(deffacts init_agent_status
    (agent_status (currently observe))
)


; passa al modulo di osservazione
(defrule go-on-observe
    (agent_status (currently observe))
 =>
    ;(printout t "------------------------------------------ I'm observing the map" crlf)
    (focus AGENT_OBSERVE)
)


; passa al modulo di piafinicazione
(defrule go-on-plan  
  (agent_status (currently plan))
=>
    (printout t crlf)
    ;(printout t "------------------------------------------ I'm planning" crlf)
    (focus AGENT_PLAN)
)


; passa al modulo di esecuzione
(defrule go-on-execute 
   (agent_status (currently execute))
 =>
    (focus AGENT_EXECUTE)
)

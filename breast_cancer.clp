(deftemplate av
    (slot attribute)
    (slot value))

(deftemplate te
   (slot attribute)
   (multislot nilai))

(deftemplate question
    (slot attribute)
    (slot text))

(deftemplate banner
  (multislot text))

(deftemplate condition
    (slot attribute)
    (slot condition1)
    (slot condition2))

(deffacts mean-concave-points-table
    (te     (attribute mean-concave-points) (nilai <= 0.05))
    (te     (attribute mean-concave-points) (nilai > 0.05)))

(deffacts questions
    (question   (attribute mean-concave-points)
                (text "mean_concave_points?")))

(defmethod checkCondition1 ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op <=))
                  (?upper INTEGER FLOAT))
   (<= ?v ?upper))
   
(defmethod checkCondition2 ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op >))
                  (?lower INTEGER FLOAT))
   (> ?v ?lower))

(defmethod ask-question ((?question STRING))
   (printout t ?question)
   (bind ?answer (read))
   (while (and (not (floatp ?answer)) (not (integerp ?answer))) do
      (printout t ?question)
      (bind ?answer (read)))
   ?answer)

(defrule print-banner
   (declare (salience 100))
   (banner (text $?text))
   =>
   (progn$ (?t ?text)
      (printout t ?t crlf)))

(deffacts banner
   (banner (text "The Framingham Risk Score estimates your 10-year risk for coronary heart disease."
                 "Your answers to a series of questions will determine your risk."
                 "")))

(defrule question
    (question   (attribute ?attribute)
                (text ?text))
    (not (av (attribute ?attribute)))
    ; (not (and (question (attribute ?attribute2&~?attribute))))
    =>
    (bind ?value (ask-question ?text))
    (assert (av (attribute ?attribute)
                (value ?value))))


(defrule add-question1
    (av (attribute ?attribute)
        (value ?value))
    (condition  (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (attribute ?attribute) (nilai $?nilai))
    ; (test (checkCondition1 ?value (expand$ ?nilai)))
    => 
    (assert (question   (attribute ?condition1) 
                        (text (format t "%s" ?condition1))))
    )

(defrule add-question2
    (av (attribute ?attribute)
        (value ?value))
    (condition  (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (attribute ?attribute) (nilai $?nilai))
    ; (test (checkCondition2 ?value (expand$ ?nilai)))
    => 
    (assert (question   (attribute ?condition2) 
                        (text (format t "%s" ?condition2))))
    )

(deffacts add-condition
    (condition (attribute mean-concave-points)
                (condition1 worst-radius) 
                (condition2 worst-perimeter)))

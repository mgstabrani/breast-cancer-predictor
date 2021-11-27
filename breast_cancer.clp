(deftemplate av
    (multislot parent_attribute)
    (slot attribute)
    (slot value))

(deftemplate te
   (multislot parent_attribute)
   (slot attribute)
   (multislot nilai)
   (slot condition))

(deftemplate question
    (multislot parent_attribute)
    (slot attribute)
    (slot text))

(deftemplate banner
  (multislot text))

(deftemplate condition
    (multislot parent_attribute)
    (slot attribute)
    (slot condition1)
    (slot condition2))

(deffacts cancer-prediction-table
    ; DEPTH 1
    ; mean_concave_points
    (te     (attribute mean_concave_points) (nilai <= 0.05) 
            (condition 1))
    (te     (attribute mean_concave_points) (nilai > 0.05) 
            (condition 2))
    
    ; DEPTH 2
    ; worst_radius
    (te     (parent_attribute mean_concave_points) 
            (attribute worst_radius) (nilai <= 16.83)
            (condition 1))
    (te     (parent_attribute mean_concave_points) 
            (attribute worst_radius) (nilai > 16.83)
            (condition 2))

    ;worst_perimeter
    (te     (parent_attribute mean_concave_points) 
            (attribute worst_perimeter) (nilai <= 114.45) 
            (condition 1))
    (te     (parent_attribute mean_concave_points) 
            (attribute worst_perimeter) (nilai > 114.45)
            (condition 2))

    ; DEPTH 3
    ; radius_error
    (te     (parent_attribute mean_concave_points worst_radius) 
            (attribute radius_error) (nilai <= 0.63) 
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius) 
            (attribute radius_error) (nilai > 0.63)
            (condition 2))

    ; mean_texture
    (te     (parent_attribute mean_concave_points worst_radius) 
            (attribute mean_texture) (nilai <= 16.19) 
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius) 
            (attribute mean_texture) (nilai > 16.19) 
            (condition 2))
    
    ; worst_texture
    (te     (parent_attribute mean_concave_points worst_perimeter) 
            (attribute worst_texture) (nilai <= 25.65) 
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_perimeter) 
            (attribute worst_texture) (nilai > 25.65)
            (condition 2))
    
    ;DEPTH 4
    ; worst_texture
    (te     (parent_attribute mean_concave_points worst_radius radius_error) 
            (attribute worst_texture) (nilai <= 30.15)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius radius_error) 
            (attribute worst_texture) (nilai > 30.15)
            (condition 2))

    ; mean_smoothness
    (te     (parent_attribute mean_concave_points worst_radius radius_error) 
            (attribute mean_smoothness) (nilai <= 0.09)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius radius_error) 
            (attribute mean_smoothness) (nilai > 0.09)
            (condition 2))

    ; concave_points_error
    (te     (parent_attribute mean_concave_points worst_radius mean_texture)
            (attribute concave_points_error) (nilai <= 0.01)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius mean_texture)
            (attribute concave_points_error) (nilai > 0.01)
            (condition 2))
    
    ; worst_concave_points
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture)
            (attribute worst_concave_points) (nilai <= 0.17)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture)
            (attribute worst_concave_points) (nilai > 0.17)
            (condition 2))
    
    ; perimeter_error
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture) 
            (attribute perimeter_error) (nilai <= 1.56)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture)
            (attribute perimeter_error) (nilai > 1.56)
            (condition 2))

    ;DEPTH 5
    ; worst_area
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture) 
            (attribute worst_area) (nilai <= 641.60)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture)
            (attribute worst_area) (nilai > 641.60)
            (condition 2))

    ; mean_radius
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture perimeter_error) 
            (attribute mean_radius) (nilai <= 13.34)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_perimeter worst_texture perimeter_error) 
            (attribute mean_radius) (nilai > 13.34)
            (condition 2))

    ; Depth 6
    ; mean_radius
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area) 
            (attribute mean_radius) (nilai <= 13.45)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area)  
            (attribute mean_radius) (nilai > 13.45)
            (condition 2))

    
    ;DEPTH 7
    ; mean_texture
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area mean_radius)
            (attribute mean_texture) (nilai <= 28.79)
            (condition 1))
    (te     (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area mean_radius) 
            (attribute mean_texture) (nilai > 28.79)
            (condition 2))
    )

(deffacts questions
    (question   (attribute mean_concave_points)
                (text "mean_concave_points")))

(defmethod checkCondition ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op <=))
                  (?upper INTEGER FLOAT))
   (<= ?v ?upper))
   
(defmethod checkCondition ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op >))
                  (?lower INTEGER FLOAT))
   (> ?v ?lower))

(defmethod ask-question ((?question STRING SYMBOL))
   (printout t ?question "? ")
   (bind ?answer (read))
   (while (and (not (floatp ?answer)) (not (integerp ?answer))) do
      (printout t ?question "? ")
      (bind ?answer (read)))
   ?answer)

(defrule print-banner
   (declare (salience 100))
   (banner (text $?text))
   =>
   (progn$ (?t ?text)
      (printout t ?t crlf)))

(deffacts banner
   (banner (text "The Breast Cancer Predictor estimates your risk for breast cancer disease."
                 "Your answers to a series of questions will determine your risk."
                 "")))

(defrule question
    (question   (parent_attribute $?parent_attribute)
                (attribute ?attribute)
                (text ?text))
    (not (av (parent_attribute $?parent_attribute) (attribute ?attribute)))
    =>
    (bind ?value (ask-question ?text))
    (assert (av (parent_attribute $?parent_attribute) (attribute ?attribute)
                (value ?value))))


(defrule add-question1
    (av (parent_attribute $?parent_attribute) (attribute ?attribute)
        (value ?value))
    (condition (parent_attribute $?parent_attribute) (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (parent_attribute $?parent_attribute) (attribute ?attribute) (nilai $?nilai) (condition ?condition))
    (test (checkCondition ?value (expand$ ?nilai)))
    (test (eq ?condition 1))
    (test (and (not (eq ?condition1 cancer)) (not (eq ?condition1 no_cancer))))
    => 
    (assert (question   (parent_attribute $?parent_attribute ?attribute)
                        (attribute ?condition1) 
                        (text ?condition1)))
    )

(defrule add-question2
    (av (parent_attribute $?parent_attribute) (attribute ?attribute)
        (value ?value))
    (condition (parent_attribute $?parent_attribute) (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (parent_attribute $?parent_attribute) (attribute ?attribute) (nilai $?nilai) (condition ?condition))
    (test (checkCondition ?value (expand$ ?nilai)))
    (test (eq ?condition 2))
    (test (and (not (eq ?condition2 cancer)) (not (eq ?condition2 no_cancer))))
    => 
    (assert (question   (parent_attribute $?parent_attribute ?attribute)
                        (attribute ?condition2) 
                        (text ?condition2)))
    )

(deffacts add-condition
    ; DEPTH 1
    (condition  (attribute mean_concave_points)
                (condition1 worst_radius) 
                (condition2 worst_perimeter))
                
    ; DEPTH 2
    (condition  (parent_attribute mean_concave_points)     
                (attribute worst_radius)
                (condition1 radius_error) 
                (condition2 mean_texture))

    (condition  (parent_attribute mean_concave_points)  
                (attribute worst_perimeter)
                (condition1 worst_texture)
                (condition2 no_cancer))
                
    ; DEPTH 3
    (condition  (parent_attribute mean_concave_points worst_radius)
                (attribute radius_error)
                (condition1 worst_texture)
                (condition2 mean_smoothness))
                
    (condition  (parent_attribute mean_concave_points worst_radius)  
                (attribute mean_texture)
                (condition1 cancer)
                (condition2 concave_points_error))

    (condition  (parent_attribute mean_concave_points worst_perimeter)  
                (attribute worst_texture)
                (condition1 worst_concave_points)
                (condition2 perimeter_error))
                
    ; DEPTH 4
    ; worst_texture
    (condition  (parent_attribute mean_concave_points worst_radius radius_error)
                (attribute worst_texture)
                (condition1 cancer)
                (condition2 worst_area))

    ; mean_smoothness
    (condition  (parent_attribute mean_concave_points worst_radius radius_error) 
                (attribute mean_smoothness)
                (condition1 cancer)
                (condition2 no_cancer))

    ; concave_points_error
    (condition  (parent_attribute mean_concave_points worst_radius mean_texture)
                (attribute concave_points_error)
                (condition1 no_cancer)
                (condition2 cancer))
    
    ; worst_concave_points
    (condition  (parent_attribute mean_concave_points worst_perimeter worst_texture)
                (attribute worst_concave_points)
                (condition1 cancer)
                (condition2 no_cancer))
                
    ; perimeter_error
    (condition  (parent_attribute mean_concave_points worst_perimeter worst_texture) 
                (attribute perimeter_error)
                (condition1 mean_radius)
                (condition2 no_cancer))

    ;DEPTH 5
    ; worst_area
    (condition  (parent_attribute mean_concave_points worst_radius radius_error worst_texture) 
                (attribute worst_area)
                (condition1 cancer)
                (condition2 mean_radius))

    ; mean_radius
    (condition  (parent_attribute mean_concave_points worst_perimeter worst_texture perimeter_error) 
                (attribute mean_radius)
                (condition1 no_cancer)
                (condition2 cancer))

    ; Depth 6
    ; mean_radius
    (condition  (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area) 
                (attribute mean_radius)
                (condition1 mean_texture)
                (condition2 cancer))
    
    ;DEPTH 7
    ; mean_texture
    (condition  (parent_attribute mean_concave_points worst_radius radius_error worst_texture worst_area mean_radius)
                (attribute mean_texture)
                (condition1 no_cancer)
                (condition2 cancer))
    )
    
(defrule result_condition1
    (av (parent_attribute $?parent_attribute) (attribute ?attribute)
        (value ?value))
    (condition (parent_attribute $?parent_attribute) (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (parent_attribute $?parent_attribute) (attribute ?attribute) (nilai $?nilai) (condition ?condition))
    (test (checkCondition ?value (expand$ ?nilai)))
    (test (eq ?condition 1))
    (test (or (eq ?condition1 cancer) (eq ?condition1 no_cancer)))
    =>
    (assert (av (parent_attribute $?parent_attribute) (attribute ?condition1)
                (value 0.0))))

(defrule result_condition2
    (av (parent_attribute $?parent_attribute) (attribute ?attribute)
        (value ?value))
    (condition (parent_attribute $?parent_attribute) (attribute ?attribute)
                    (condition1 ?condition1)
                    (condition2 ?condition2))
    (te (parent_attribute $?parent_attribute) (attribute ?attribute) (nilai $?nilai) (condition ?condition))
    (test (checkCondition ?value (expand$ ?nilai)))
    (test (eq ?condition 2))
    (test (or (eq ?condition2 cancer) (eq ?condition2 no_cancer)))
    =>
    (assert (av (parent_attribute $?parent_attribute) (attribute ?condition2)
                (value 1.0))))

(defrule result_cancer
    (av (parent_attribute $?parent_attribute) (attribute ?attribute))
    (test (eq ?attribute cancer))
    =>
    (printout t crlf "Hasil Prediksi = Terprediksi Kanker Payudara" crlf))

(defrule result_no_cancer
    (av (parent_attribute $?parent_attribute) (attribute ?attribute))
    (test (eq ?attribute no_cancer))
    =>
    (printout t crlf "Hasil Prediksi = Terprediksi Tidak Kanker Payudara" crlf))


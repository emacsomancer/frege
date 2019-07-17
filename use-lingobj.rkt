#lang s-exp "lingobj.rkt"

;; type e definitions
(defden john e #:abstract)
(defden bill e #:abstract)
(defden sally e #:abstract)
(defden mary e #:abstract)

;; type <e,t> definitions
(defden drinks (-> e t) #:abstract)
(defden man (-> e t) #:abstract)
(defden F (-> e t)     ; arbitrary function definition for 'drinks'
  (λ (x) (drinks x)))

;; type <e,<e,t>> definitions
(defden loves (-> e (-> e t)) #:abstract)

;; type <e,<e,<e,t>>> definitions
(defden introduces (-> e (-> e (-> e t))) #:abstract)

;; type <<e,t>,t> definitions
(defden mary-gq (-> (-> e t) t)   ; 'generalised quantifier' version of Mary
  (λ (P) (P mary)))

;; type <<e,t>,<<e,t>,t>> definitions
(defden every (-> (-> e t) (-> (-> e t) t))
  (λ (P)
    (λ (Q)
      (∀ ([x e])
         (if (P x) (Q x))))))



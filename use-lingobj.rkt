#lang s-exp "lingobj.rkt"

;; type e definitions
(defden john e #:uninterpreted)
(defden bill e #:uninterpreted)
(defden sally e #:uninterpreted)
(defden mary e #:uninterpreted)

;; type <e,t> definitions
(defden drinks (-> e t) #:uninterpreted)
(defden snores (-> e t) #:uninterpreted)
(defden man (-> e t) #:uninterpreted)
(defden F (-> e t)     ; arbitrary function definition for 'drinks'
  (λ (x) (drinks x)))

;; type <e,<e,t>> definitions
(defden loves (-> e (-> e t)) #:uninterpreted)

;; type <e,<e,<e,t>>> definitions
(defden introduces (-> e (-> e (-> e t))) #:uninterpreted)

;; type <<e,t>,t> definitions
(defden mary-gq (-> (-> e t) t)   ; 'generalised quantifier' version of Mary
  (λ (P) (P mary)))

;; type <<e,t>,<<e,t>,t>> definitions
(defden every (-> (-> e t) (-> (-> e t) t))
  (λ (P)
    (λ (Q)
      (∀ ([x e])
         (when (P x) (Q x))))))

;; misc. testing defs.
(defden test01 (-> e (-> (-> e t) t))
  (λ (x)
    (λ (P)
      (P x))))

(defden test02 (-> e (-> (-> e t ) (-> (-> e t) t)))
  (λ (x)
    (λ (P)
      (λ (Q)
        (when (P x) (Q x))))))

(defden test03 (-> e (-> (-> e t) t))
  (λ ([x e])
    (λ (P)
      (P x))))

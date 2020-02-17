;; Frege: derivational representations of natural language semantics, using a typed lambda calculus, implemented in Racket Scheme.
;; Copyright (C)  2019-2020 Benjamin Slade, Jesse A. Tov 

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

#lang s-exp "lingobj.rkt"

;; type e definitions
(defden john e #:uninterpreted)
(defden bill e #:uninterpreted)
(defden sally e #:uninterpreted)
(defden mary e #:uninterpreted)

;; type <e,t> definitions
; really shouldn't be uninterpreted, so define 'dummy' uninterpreted preds to refer to (with Uppercase)
(defden Drinks (-> e t) #:uninterpreted)
(defden drinks (-> e t) 
  (λ (x)
    (Drinks x))) 

(defden Snores (-> e t) #:uninterpreted)
(defden snores (-> e t) 
  (λ (x)
    (Snores x))) 

(defden Man (-> e t) #:uninterpreted)
(defden man (-> e t) 
  (λ (x)
    (Man x))) 

;; type <e,<e,t>> definitions
(defden Loves (-> e (-> e t)) #:uninterpreted) 
(defden loves (-> e (-> e t))                  
  (λ (y)
    (λ (x)
      (y (Loves x)))))

;; type <e,<e,<e,t>>> definitions
(defden Introduces (-> e (-> e (-> e t))) #:uninterpreted)
(defden introduces (-> e (-> e (-> e t)))
  (λ (z)
    (λ (y)
      (λ (x)
        (x ((Introduces x) z))))))


;; type <<e,t>,t> definitions
(defden mary-gq (-> (-> e t) t)   ; 'generalised quantifier' version of Mary
  (λ (P) (P mary)))

;; type <<e,t>,<<e,t>,t>> definitions
(defden every (-> (-> e t) (-> (-> e t) t))
  (λ (P)
    (λ (Q)
      (∀ ([x e])
         (when (P x) (Q x))))))

;; type <<e,t>,<<e,t>,t>> definitions
(defden some (-> (-> e t) (-> (-> e t) t))
  (λ (P)
    (λ (Q)
      (∃ ([x e])
         (and (P x) (Q x))))))


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

(defden |Everyone who drinks loves Bill.|
  ((every drinks) (λ ([who e]) ((who loves) bill))))

(defden |Everyone who loves Bill drinks.|
  ((every (λ ([who e]) ((who loves) bill))) drinks))

(defden |Everyone who Bill loves drinks.|
  ((every (bill loves)) drinks))

(defden |Some man snores.|
  ((some man) snores))

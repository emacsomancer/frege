#lang turnstile/lang

(provide (rename-out [L:#%app #%app]
                     [λ       lambda])
         λ and or when ∀ ∃ not
         defden
         (rename-out [#%app $]) lingobj-den
         (type-out e t ->))

(define-base-types e t)
(define-type-constructor -> #:arity = 2)

(module run-time racket/base
  (provide (all-defined-out))
  (require racket/match)
  (require (for-template (only-in turnstile/base type->str)))

  ; A LingObj is (make-lingobj type? Denotation)
  ;
  ;   where
  ;
  ; A Denotation is one of:
  ;  - (lam [Binder 1])
  ;  - (app Denotation Denotation)
  ;  - (conj Denotation Denotation)
  ;  - (impl Denotation Denotation)
  ;  - (neg Denotation)
  ;  - (all [Binder n])
  ;  - (exists [Binder n])  
  ;
  ; A [Binder n] is (make-binder [list symbol? ...n]
  ;                              [list type? ...n]
  ;                              [[list symbol? ...n] -> Denotation])
  
  (struct lingobj [type den]
    #:methods gen:custom-write
    [(define (write-proc obj port mode)
       (display (obj->sexp obj) port)
       (display ":" port)
       (display (type->str (lingobj-type obj)) port))])

  (struct binder [names types proc])

  (define (obj->sexp obj)
    (define den (lingobj-den obj))
    (match den
      [(lam b)         (binder->sexp 'λ b)]
      [(app d1 d2)     `(,d1 ,d2)]
      [(conj d1 d2)    `(and ,(obj->sexp d1) ,(obj->sexp d2))]
      [(disj d1 d2)    `(or ,(obj->sexp d1) ,(obj->sexp d2))]      
      [(impl d1 d2)    `(if ,(obj->sexp d1) ,(obj->sexp d2))]
      [(neg d1)        `(not ,(obj->sexp d1))]
      [(all d1)        (binder->sexp '∀ d1)]
      [(exists d1)     (binder->sexp '∃ d1)]
      [(? symbol?)     den]
      [any             (error 'obj->sexp "not a valid lingobj: ~a" den)]))

  (define (binder->sexp head binder)
    (list head
          (map (λ (x t) (list x (type->str t)))
               (binder-names binder)
               (binder-types binder))
          (apply (binder-proc binder) (binder-names binder))))
  
  (struct lam [b])
  (struct app [d1 d2])
  (struct conj [d1 d2])
  (struct disj [d1 d2])  
  (struct impl [d1 d2])
  (struct neg [d])
  (struct all [b])
  (struct exists [b]))


(require 'run-time)

(define-syntax define-ling-syntax/lift
  (syntax-parser
    [(_ (external:id d0:id di:id ...) internal:id)
     #:with (d0- di- ...) (generate-temporaries #'(d0 di ...))
     #'(define-typed-syntax external
         [(_ d0 di ...)
          ⇐ τ
          ≫
          [⊢ d0 ≫ d0- ⇐ τ]
          [⊢ di ≫ di- ⇐ τ] ...
          ----
          [⊢ (lingobj #'τ (internal d0- di- ...))]]
         
         [(_ d0 di ...)
          ≫
          [⊢ d0 ≫ d0- ⇒ τ]
          [⊢ di ≫ di- ⇐ τ] ...
          ----
          [⊢ (lingobj #'τ (internal d0- di- ...)) ⇒ τ]])]))

(define-syntax define-binding-ling-syntax/lift
  (syntax-parser
    [(_ (external:id (bv:id ...+) body:id) internal:id)
     #:with (bv- ...) (generate-temporaries #'(bv ...))
     #:with (σ ...) (generate-temporaries #'(bv ...))
     #:with (σ.norm ...) (stx-map (λ (σ) (format-id σ "~a.norm" σ)) #'(σ ...))
     #'(define-typed-syntax external
         [(_ ([(~var bv id) (~var σ type)] ...) (~var body expr))
          ⇐ τ
          ≫
          [[bv ≫ bv- : σ.norm] ... ⊢ body ≫ body- ⇐ τ]
          ----
          [⊢ (lingobj #'τ (internal (binder '(bv ...)
                                            (list #'σ.norm ...)
                                            (λ- (bv- ...) body-))))]]
         
         [(_ ([(~var bv id) (~var σ type)] ...) (~var body expr))
          ≫
          [[bv ≫ bv- : σ.norm] ... ⊢ body ≫ body- ⇒ τ]
          ----
          [⊢ (lingobj #'τ (internal (λ- (bv- ...) body-))) ⇒ τ]])]))

(define-typed-syntax λ
  [(_ (x:id) body:expr)
   ⇐ (~-> dom cod)
   ≫
   [[x ≫ x- : dom] ⊢ [body ≫ body- ⇐ cod]]
   ----
   [⊢ (lingobj #'(-> dom cod)
               (lam (binder '(x) (list #'dom) (λ- (x-) body-))))]]

  [(_ ([x:id τ:type]) body:expr)
   ⇐ (~-> dom cod)
   ≫
   #:fail-unless (type=? #'dom #'τ.norm)
   (format "expected domain ~a ≠ annotation ~a"
           (type->str #'dom) (type->str #'τ.norm))
   [[x ≫ x- : dom] ⊢ [body ≫ body- ⇐ cod]]
   ----
   [⊢ (lingobj #'(-> dom cod)
               (lam (binder '(x) (list #'dom) (λ- (x-) body-))))]]
  
  [(_ ([x:id τ:type]) body:expr)
   ≫
   #:with dom #'τ.norm
   [[x ≫ x- : dom] ⊢ [body ≫ body- ⇒ cod]]
   ----
   [⊢ (lingobj #'(-> dom cod)
               (lam (binder '(x) (list #'dom) (λ- (x-) body-))))
      ⇒ (-> dom cod)]])


(define-typed-syntax L:#%app
  [(_ d1:expr d2:expr)
   ≫
   [⊢ d1 ≫ d1- ⇒ τ1]
   [⊢ d2 ≫ d2- ⇒ τ2]
   #:with τ (syntax-parse (cons #'τ1 #'τ2)
              [((~-> τ1-dom τ1-cod) . _)
               #:when (type=? #'τ1-dom #'τ2)
               #'τ1-cod]
              [(_ . (~-> τ2-dom τ2-cod))
               #:when (type=? #'τ2-dom #'τ1)
               #'τ2-cod]
              [any (raise-syntax-error
                    '#%app
                    (format "mismatch: left=~a right=~a"
                            (type->str #'τ1) (type->str #'τ2)))])
   ----
   [⊢ (lingobj #'τ (app d1- d2-)) ⇒ τ]])


(define-ling-syntax/lift (and d1 d2) conj)
(define-ling-syntax/lift (or d1 d2) disj)
(define-ling-syntax/lift (when d1 d2) impl)
(define-ling-syntax/lift (not d1) neg)


(define-binding-ling-syntax/lift (∀ (x) d1) all)
(define-binding-ling-syntax/lift (∃ (x) d1) all)


(define-typed-syntax defden
  [(_ x:id τ:type #:uninterpreted)
   ≫
   ----
   [≻ (define-typed-variable x (lingobj #'τ.norm (quote- x)) ⇒ τ.norm)]]
  [(_ x:id τ:type e:expr)
   ≫
   ----
   [≻ (define-typed-variable x e ⇐ τ.norm)]]
  [(_ x:id e:expr)
   ≫
   ----
   [≻ (define-typed-variable x e)]])
   

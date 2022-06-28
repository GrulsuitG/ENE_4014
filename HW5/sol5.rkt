#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;extra problem
(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (let var = e in body)

(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to racket int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error messaeg (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to num-array-var, e2 evaluates to racket int (index of the value to access), and e3 evaluates to a MUPL int
                                              ;; (num-array-set (num-array 4) 0 (int 42))
                                              ;; (num-array-set (num-array 4) 5 (int 42)) ; this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 (int 42)) ; this should give a nice error messaeg (like "array access out of bound")



(define (make-array length)
    (if (= length 0)
        null
        (mcons (int 0) (make-array (- length 1)))))
;; (define (set-array-val array index val)
;;   (if (= index 0)
;;      (set-mcar! array val)
;;      (mcar array)))
;; Problem 1

(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (aunit)]
        [(pair? rlist) (apair (car rlist) (racketlist->mupllist (cdr rlist)))]
        [(#t (error "given list is not racketlist"))]))

(define (mupllist->racketlist mlist)
  (cond [(aunit? mlist) null]
        [(apair? mlist) (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))]
        [#t (error "given list is not mupllist")]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([fe (eval-under-env (call-funexp e) env)]
               [ac (eval-under-env (call-actual e) env)])
           (if (closure? fe)
               (let* ([func (closure-fun fe)]
                     [fenv (cons (cons (fun-formal func) ac) (closure-env fe))])
                 (if (fun-nameopt func)
                     (let ([fenv (cons (cons (fun-nameopt func) fe) fenv)])
                       (eval-under-env (fun-body func) fenv))
                     (eval-under-env (fun-body func) fenv)))
               (error "first arg is not clousre")))]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
               [en (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) en))]
        [(glet? e)
         (define (make-env env value name)
           (cond [(null? env) null]
                 [#t (if (string=? (caar env) name)
                         (cons (cons name value) (make-env (cdr env) value name))
                         (cons (cons (caar env) (cdar env)) (make-env (cdr env) value name)))]))
           
         (define (env-traver env)
           (let* ([value (eval-under-env (glet-e e) env)]
                  [name (glet-var e)])
             (cond [(null? env) null]
                   [(closure? (cdar env))
                    (let ([cenv (closure-env (cdar env))])
                      (cons (cons (caar env) (closure (make-env cenv value name) (closure-fun (cdar env))))
                            (env-traver (cdr env))))]
                      
                   [#t (if (string=? (caar env) name)
                        (cons (cons name value) (env-traver (cdr env)))
                        (cons (cons (caar env) (cdar env)) (env-traver (cdr env))))])))
         (let* ([value (eval-under-env (glet-e e) env)]
                [name (glet-var e)])
           (eval-under-env (glet-body e) (cons (cons name value) (env-traver env))))]
               
        [(apair? e)
         (let ([p1 (eval-under-env (apair-e1 e) env)]
               [p2 (eval-under-env (apair-e2 e) env)])
           (apair p1 p2))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
               (if (apair? p)
                   (apair-e1 p)
                   (error "MUPL fst applied to non-apair")))]
           ;(error "argument must be a pair"))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
               (if (apair? p)
                   (apair-e2 p)
                   (error "MUPL fst applied to non-apair")))]
          ;(error "argument must be a pair"))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [(num-array? e)
         (if (number? (num-array-size e))
             (make-array-object (num-array-size e))
             (error "given value is not int"))]
        [(num-array-at? e)
         (define (traversal array time)
           (if (= 0 time)
               (mcar array)
               (traversal (mcdr array) (- time 1))))
         (let ([arr (eval-under-env (num-array-at-e1 e) env)]
               [index (num-array-at-e2 e)])
           (if (num-array-object? arr)
               (if (or (< index 0) (<= (array-length arr) index))
                   (error "array access out of bound")
                   (traversal arr index))
               (error "not array object")))]
        [(num-array-set? e)
         (let ([arr (eval-under-env (num-array-set-e1 e) env)]
               [index (num-array-set-e2 e)]
               [var (num-array-set-e3 e)])
           (if (and (num-array-object? arr) (int? var))
               (if (or (< index 0) (<= (array-length arr) index))
                   (error "array access out of bound")
                   (begin (set-array-val arr index var) var))
               (error "not array object or not mupl int")))]
               
                    
         
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([hd (car lstlst)])
        (mlet (car hd) (cdr hd) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))
                    

;; Problem 4

(define mupl-map
  (fun "mupl-mapf" "fun"
       (fun "maplst" "l"
            (ifaunit (var "l")
                      (aunit)
                      (apair (call (var "fun") (fst (var "l"))) (call (var "maplst") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mapADDN" "i"
             (call (var "map") (fun "addi" "x" (add (var "i") (var "x")))))))



(define (num-array-object? v) ;; hackish implementation of testing num-array object. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))
(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))
(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))

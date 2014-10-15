(define interpret #f)

(define top-level-env `( (+ ,+) (- ,-) (1 ,1) )) ;Paired like (symbol, actual procedure)

;Looks up the given id in the top-level-environment and returns its procedural value
;identifiers are symbols, so they need a ' in front
;Ex: (top-level-value '+) => #[procedure 15]
(define top-level-value 
              (lambda (id) 
                  (let (( pair (assq id top-level-env)))
                        (if pair
                            (cadr pair)
                            (id)))))

;Adds pairing to the top-level-env
(define set-top-level-value! 
              (lambda (id val) 
                  (let (( pair (assq id top-level-env)))
                        (if (null? pair)
                            (id)
                            (set! top-level-env (cons (list id val) top-level-env))))))

;----Current Idea
;(define expression '()) and cons it with the broken down statements
;Worked with an if statement and its clauses
;Possible issues: resetting the list after each use
;Breaks an exp down to its core statements
;How do you read an expression and split it into pairs?
(define expand (lambda (exp) 
    (cond 
        ((eqv? (car exp) 'cond) 
        	(let* ((e1 (cadr exp)) 
        		(cond
        			((eqv? (cdr e1) '()) (<or sequence>)) ;if the cdr is empty, 'or' cond
        			(else <normal sequence>))	;else, normal cond
        ((eqv? (car exp) 'case) (<sequence>))
        ((eqv? (car exp) 'and) (<seq>))
        ((eqv? (car exp) 'or) (<seq>))
        ((eqv? (car exp) 'let) (<seq>))
        ((eqv? (car exp) 'let*) (<seq>))
        ((eqv? (car exp) 'letrec) (<seq>))
        (else exp)
    ))


;replace the following "..."'s with "id))" to make them dummy functions
;"idl))" for new-env, so that you can load the code.

(letrec
;New-Env - Creates a new environment from given ids and vals? Or maybe this adds them to the given one.
   ((new-env
    (lambda (idl vals env)
                idl))

;Lookup - searches for the id in the environment
   (lookup
    (lambda (id env)
                id))
  
;Assign - Makes a symbol = value?
   (assign
    (lambda (id val env)
		id))	;Currently dummy functions

   (exec
    (lambda (exp env)
      (cond
           ((symbol? exp) (lookup exp env))
           ((number? exp) (lookup exp env))
           ((pair? exp) 	;technically, everything's a pair of a car and cdr
             (case  (car exp)
                   ((quote) (cadr exp))
                   ((lambda)
                          (lambda vals
                                 (exec (cons 'begin (cddr exp))
                                           (new-env (cadr exp) vals env))))
                   ((if) 
                          (if (exec (cadr exp) env)
                              (exec (caddr exp) env)
                              (exec (cadddr exp) env)))
                   ((set!)
                          (assign (cadr exp)
                                      (exec (caddr exp) env)
                                       env))
                   ((begin)
                          (let loop ((exps (cdr exp)))
                              (if (null? (cdr exps))
                                  (exec (car exps) env)
                                  (begin
                                       (exec (car exps) env)
                                       (loop (cdr exps))))))
                   (else
                          (apply (exec (car exp) env)
                                    (map (lambda (x)  (exec x env))
                                             (cdr exp))))))
           (else exp)))))

(set! interpret
      (lambda (exp)
            (exec (expand exp) '())))) 
            ;'() is the environment the grader will be giving
            ;top-level-env is an example

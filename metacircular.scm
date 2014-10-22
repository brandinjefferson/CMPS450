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

;Test - (cond ((eqv? 2 2) (+ 1 2)) ((eqv? 1 1) (+ 1 1)))
;----Current Idea
;(define expression '()) and cons it with the broken down statements
;Worked with an if statement and its clauses
;Possible issues: resetting the list after each use
;Breaks an exp down to its core statements
;How do you read an expression and split it into pairs?
(define expand (lambda (exp) (helper exp '())))

(define helper (lambda (e1 e2)
    (if (equal? e1 '())
        e2
    (cond
        ((eqv? (car e1) 'cond)
            (if (not (null? (cdr (cadr e1)))) ;if sequence exists
                ;(cond <t> <seq>)
                (if (null? (cddr e1)) ;Then this is the last cond
                    (if (null? e2) (append e2 (list 'if (caadr e1) (cons 'begin (cdr (cadr e1))))) (append e2 (list (list 'if (caadr e1) (cons 'begin (cdr (cadr e1)))))))
                    (helper (cons 'cond (cddr e1)) (append e2 (list 'if (caadr e1) (cons 'begin (cdr (cadr e1)))))))
                ;else (cond (<t>) <clause> ...)
                (helper (cons 'cond (cddr e1)) (append e2 (list 'or (caadr exp) (cons 'begin (cdr (cadr e1)))))))
            )
        (else (helper (cdr e1) (append e2 (car e1))))))))


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

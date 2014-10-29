(define interpret #f)

(define top-level-env `( (+ ,+) (- ,-) (1 ,1) (#f ,#f) (#t ,#t) (eqv? ,eqv?))) ;Paired like (symbol, actual procedure)

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


(define expand (lambda (exp) (helper exp '())))

(define helper (lambda (e1 e2)
    (if (or (equal? e1 '()) (not (pair? e1)) (equal? e1 e2))
        e2
    (cond
        ((eqv? (car e1) 'cond)
            (if (not (null? (cdr (cadr e1)))) ;if sequence exists
                ;(cond <t> <seq>)
                (if (null? (cddr e1)) ;Then this is the last cond
                    (if (null? e2) (append e2 (list 'if (caadr e1) (cons 'begin (cdr (cadr e1))))) (append e2 (list (list 'if (caadr e1) (cons 'begin (cdr (cadr e1)))))))
                    (helper (cons 'cond (cddr e1)) (append e2 (list 'if (caadr e1) (cons 'begin (cdr (cadr e1)))))))
                ;else (cond (<t>) <clause> ...)
                (helper (list 'or (caadr e1) (cons 'cond (cddr e1))) e2)))
        ((eqv? (car e1) 'and) 
            (if (not (null? (cdr e1)))        ;(and <t1>)
                (if (not (null? (cddr e1)))   ;(and <t1> <t2> ...)
                    (append e2 (list 'let (list (cons 'x (list (cadr e1))) (list 'thunk (list 'lambda '() (helper (cons 'and (cddr e1)) e2)))) (list 'if 'x '(thunk) 'x)))
                    ;else return <t1>
                    (append e2 (helper (cdr e1) e2)))
                (append e2 '#t)))
        ((eqv? (car e1) 'or) 
            (if (not (null? (cdr e1)))        ;(or <t1>)
                (if (not (null? (cddr e1)))   ;(or <t1> <t2> ...)
                    (append e2 (helper (list 'let (list (cons 'x (list (cadr e1))) (list 'thunk (list 'lambda '() (helper (cons 'or (cddr e1)) e2)))) (list 'if 'x 'x '(thunk))) e2))
                    ;else return <t1>
                    (append e2 (if (null? (cddr e1)) (helper (cadr e1) e2) (helper (cdr e1) e2))))
                (append e2 '#f)))
        ((eqv? (car e1) 'case)
         ;Still messing up somewhere
             (helper (list 'let (cons (list 'key (cadr test)) (casesequences (cddr test) '() 1)) (list 'cond (caselists (cddr test) '() 1))) '()))  
        ((eqv? (car e1) 'let)
            (append e2 (cons (list 'lambda (getvar (cadr e1) '()) (helper (caddr e1) '())) (getinit (cadr e1) '()))))
        ((eqv? (car e1) 'let*)
            (if (null? (cadr e1))
                ;(let* () <body>)
                (append e2 (list 'lambda '() (helper (cddr e1) '())))
                ;(let* ((v1 init1) (v2 init2) ...) <body>)
                (helper (list 'let (list (caadr e1)) (list 'let* (cdr (cadr e1)) (caddr e1))) e2)))
        ((eqv? (car e1) 'letrec)
             (helper (list (list 'let  (getvarrec (cadr e1) '())) (list 'let (getinitrec (cadr e1) '() 1)) (list 'begin (getcombos (cadr e1) '() 1) (helper (caddr e1) '()))) e2))
        (else (helper (if (null? (cdr e1)) (car e1) (cdr e1)) (if (pair? (car e1)) 
                                                                  (append e2 (car e1))
                                                                  (append e2 (list (car e1))))))))))

(define getvar (lambda (e1 e2) ;e1 = set of sequences
                 (if (or (not (null? e1)))
                     (getvar (cdr e1) (append e2 (list (caar e1))))
                     e2)))

(define getinit (lambda (e1 e2)
                  (if (not (null? e1))
                      (if (pair? (car (cdar e1))) 
                          (getinit (cdr e1) (append e2 (cons (helper (car (cdar e1)) '()) '())))
                          (getinit (cdr e1) (append e2 (helper (list (car (cdar e1))) '()))))
                      e2)))

(define getvarrec (lambda (e1 e2)
                    (if (not (null? e1))
                     (getvarrec (cdr e1) (append e2 (list (cons (caar e1) '(undefined))))) ;could turn this into just (list (caar e1) 'undefined)?
                     e2)))

(define getinitrec (lambda (e1 e2 num)
                     (if (not (null? e1))
                         (if (pair? (car (cdar e1))) 
                          (getinitrec (cdr e1) (append e2 (list `,(string->symbol (string-append "temp" (number->string num))) (cons (helper (list (car (cdar e1))) '()) '()))) (+ num 1))
                          (getinitrec (cdr e1) (append e2 (list `,(string->symbol (string-append "temp" (number->string num))) (helper (list (car (cdar e1))) '()))) (+ num 1)))
                      e2)))

(define getcombos (lambda (e1 e2 num) 
                    (if (not (null? e1))
                        (getcombos (cdr e1) (append e2 (list 'set! (caar e1) `,(string->symbol (string-append "temp" (number->string num))))) (+ num 1))
                        e2)))

(define casesequences (lambda (e1 e2 num) 
                        (if (null? e1)
                            e2
                            (if (eqv? (caar e1) 'else)
                                (append e2 (list 'elsethunk (list 'lambda '() (helper (cdar e1) '()))))
                                (casesequences (cdr e1) (append e2 (list (list `,(string->symbol (string-append "thunk" (number->string num))) (list 'lambda '() (helper (cdar e1) '()))))) (+ num 1))))))

(define caselists (lambda (e1 e2 num)
                    (if (null? e1)
                        e2
                        (if (eqv? (caar e1) 'else)
                            (append e2 (list (list (list 'quote (caar e1)) (list 'elsethunk))))
                            (caselists (cdr e1) (append e2 (list (list (list 'memv 'key (list 'quote (caar e1))) (list `,(string->symbol (string-append "thunk" (number->string num))))))) (+ num 1)))))) 

(define test '(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite)))
;replace the following "..."'s with "id))" to make them dummy functions
;"idl))" for new-env, so that you can load the code.

(letrec
;New-Env - Creates a new environment from given ids and vals? Or maybe this adds them to the given one.
   ((new-env
    (lambda (idl vals env)
                (if (null? idl)
                    env
                    (new-env (cdr idl) (cdr vals) (append env (list (car idl) (car vals)))))))

;Lookup - searches for the id in the environment and returns its value (cdr)
   (lookup
    (lambda (id env)
                (let ((pair (assq id env)))
                  (if pair (cadr pair) id))))
  
;Assign - Makes a symbol = value?
   (assign
    (lambda (id val env)
      (let ((pair (list id val))) 
        (append env (list pair)))))

   (exec
    (lambda (exp env)
      (cond
           ((symbol? exp) (lookup exp env))
           ((number? exp) (lookup exp env))
           ((pair? exp) 	
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
            (exec (expand exp) top-level-env)))) 

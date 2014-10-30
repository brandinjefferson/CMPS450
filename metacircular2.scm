#|
Author: Brandin Jefferson

Problem Description
Name: Meta-circular Interpreter
Problem Statement: The following program, written in Scheme, is meant to partially enact the features of a meta-circular interpreter for the Scheme language. It receives input, breaks it down into its core expression equivalent, and returns the procedures that are being used within the given statement. The resulting expression will do exactly what the original expression using derived expressions would do.
Input: A quoted expression is passed as the sole argument to the function interpret. In addition, a top level environment must be hard coded into the call to exec before running the operation, else the result will be an error.
Output: The result of the expression or the procedures used within it.
|#

(define interpret #f)

(define top-level-env `( (+ ,+) (- ,-) (1 ,1) (eqv? ,eqv?) (2 ,2) (3 ,3) (0 ,0) (4 ,4) (memv ,memv) (equal? ,equal?) (* ,*) ))

(define top-level-value 
              (lambda (id) 
                  (let (( pair (assq id top-level-env)))
                        (if pair
                            (cadr pair)
		            (id)))))

(define set-top-level-value! 
              (lambda (id val) 
                  (let (( pair (assq id top-level-env)))
                        (if (null? pair)
                            (id)
                            (set! top-level-env (cons (list id val) top-level-env))))))


(define expand (lambda (exp) (helper exp '())))

;Name: helper
;Arguments: e1 – A quoted expression that would work correctly if run normally; e2 – This list should be empty if being called for the first time.
;Description: Checks the car for the type of expression and places it within a temporary variable. If it is a cond, case, let, letrec, let*, and, or an or then the function enters the conditional that applies to that expression. Inside each conditional, the appropriate steps are taken to create an expression that is equivalent to the derived expression and summarily appended to a list. Through recursion, nested functions also have their correct values outputted in the correct location. If none of the previously named expressions are equal to the head of the given expression, then it is appended to whatever portion of the list it is a part of and the helper function is called on the tail of the expression. This continues until an empty set, number, or something else that is not a pair is encountered, where the function returns the statements it has created.
;Precondition: A quoted expression and a list
;Postcondition: The same expression with all derived expressions replaced with their core equivalents
;Cost Analysis: O(n)
(define helper (lambda (exp final)
                 (if (or (null? exp) (not (pair? exp)))
                     final
                     (let* ((e1 (memv 'cond exp)) (e2 (memv 'and exp))(e3 (memv 'or exp))(e4(memv 'let exp))(e5(memv 'let* exp))(e6(memv 'letrec exp))(e7(memv 'case exp)) 
                                                 (e8(memv 'lambda exp)) (e9 (if (and (pair? (car exp)) (memv 'lambda (car exp))) exp #f)))
                         (cond 
                            (e1 (cond ((null? (cdr (cadr e1))) 
                                       ;(cond (<t>) <clause2> ...) = (or <t> (cond <clause2> ...))
                                       (let ((this `(or (,(caadr e1)) (cond (,(cddr e1)))))) (helper this final)))
                                       ;(cond (<clause1>) (<clause2) ...) = (if (test1) (begin (<sequence1>)) (cond (<clause2>)...) 
                                       (else (let ((this (if (not (null? (cddr e1)))
                                                             `(if ,(helper `,(caadr e1) '()) (begin (,@(helper (cadr (cadr e1)) '()))) ,(if (eqv? (car (caddr e1)) 'else)
                                                                                                                                `(begin ,(helper (cadr (caddr e1)) '()))
                                                                                                                                `,(helper `(cond ,(car (cddr e1))) '())))
                                                             `(if ,(helper `,(caadr e1) '()) (begin (,@(helper (cadr (cadr e1)) '()))))))) (append final this)))))
                            (e2 (cond ((not (null? (cdr e2))) 
                                       (if (null? (cddr e2)) 
                                           ;(and (<t>)) = <t>
                                           (helper (cadr e2) '())
                                           ;(and <t1> <t2> ...) = (let ((x <t1>) (thunk (lambda () (and <t2> ...)))) (if x (thunk) x))
                                           (let ((this `(let ((x ,(cadr e2)) (thunk (lambda () ,(helper `(and ,(cddr e2)) '())))) (if x (thunk) x)))) (helper this final))))
                                      ;(and)
                                      (else (append final '#f))))
                            (e3 (cond ((not (null? (cdr e3))) 
                                       (if (null? (cddr e3)) 
                                           ;(or (<t>)) = <t>
                                           (helper (cadr e3) '())
                                           ;(or <t1> <t2> ...) = (let ((x <t1>) (thunk (lambda () (or <t2> ...)))) (if x x (thunk)))
                                           (let ((this `(let ((x ,(cadr e3)) (thunk (lambda () ,(helper `(or ,(cddr e3)) '())))) (if x x (thunk))))) (helper this final))))
                                      ;(or)
                                      (else (append final '#t))))
                            (e4 ;(let ((v1 init1) ...) <body>) = ((lambda (v1 ...) <body>) init1 ...)
                             (append final `((lambda ,(getvar (cadr e4) '()) ,(helper (caddr e4) '())) ,@(getinit (cadr e4) '()))))
                            (e5 (if (null? (cadr e5))
                                       ;(let* () <body>) = ((lambda () <body>))
                                       (append final `((lambda () ,(helper (caddr e5) '()))))
                                       ;(let* ((v1 init1) ...) <body>) = (let ((v1 init1)) (let* ((v2 init2) ...) <body>)
                                       (let ((this `(let (,(caadr e5)) (let* ,(cdr (cadr e5)) ,(caddr e5))))) (append final (helper this final)))))
                            (e6 ;(letrec ((v1 init1) ...) <body>) = (let ((v1 undefined) ...) (let ((temp1 init1) ...) (set! v1 temp1) ...) <body>)
                             (append final (helper `(let (,(getvarrec `,(cadr e6) '())) (let (,(getinitrec `,(cadr e6) '() 1)) (begin ,(getcombos `,(cadr e6) '() 1))) ,(helper (caddr e6) '())) final)))
                            (e7 ;(case (key) ((d1 ...) (sequence)) ... (else f1 f2 ...)) = (let ((key <key>) (thunk1 (lambda () <seq>)) ... (elsethunk (lambda () f1 f2 ...))) (cond ((memv key '(d1 ...)) (thunk1)) ... (else (elsethunk))))
                             (helper `(let ((key ,(cadr e7)) ,(casesequences `,(cddr e7) '() 1)) ,(helper (cond (`,(caselists `,(cddr e7) '() 1))) '())) '()))
                            ;Lambda help
                            (e8 (append final `(lambda ,(cadr e8) ,(helper (caddr e8) '()))))
                            (e9 (append final `((lambda ,(if (null? (cdr e9)) (car e9) (cadr e9)) ,(helper (car e9) '()) ,@(cdr e9)))))
                            ;Other
                            (else (helper (if (null? (cdr exp)) (if (and (pair? (car exp)) (not (eqv? (length (car exp)) 1))) 
                                                                        (caar exp) (car exp)) (cdr exp)) (append final (list (car exp)))))))
                         )))

#|
Name: casesequences
Arguments: e1 – A list of sequences; e2 – a list; num – the current temp variable number in use
Description: Used in the case conditional, this takes every sequence within the case statement, converts the sequence into a lambda expression,  and makes it the tail of a pair with the head being “thunk” + num. 
Precondition: A list of sequences, an empty list, and the number 1
Postcondition: A list of all sequences in the form (thunk+num (lambda () <sequence>))
Cost Analysis: O(n)
|#
(define casesequences (lambda (e1 e2 num)
                        (if (null? e1)
                            e2
                            (if (eqv? (caar e1) 'else)
                                (append e2 `(elsethunk (lambda () ,(helper `,(cdar e1) '()))))
                                (casesequences (cdr e1) (append e2 (list `(,(string->symbol(string-append "thunk" (number->string num)))) `((lambda () ,(helper `,(cdar e1) '()))))) (+ num 1))))))

#|
Name: caselists
Arguments: e1 – A list of sequences; e2 – a list; num – the current temp variable number in use
Description: Takes the list of sequences and places each in the format ((memv key (sequence)) ((thunk+num)))
Precondition: A list of sequences, an empty list, and the number 1
Postcondition: A list of items in the form ((memv key (sequence)) ((thunk+num)))
Cost Analysis: O(n)
|#
(define caselists (lambda (e1 e2 num)
                    (if (null? e1)
                        e2
                        (if (eqv? (caar e1) 'else)
                            (append e2 `(else (elsethunk)))
                            (caselists (cdr e1) (append e2 `(memv key (quote ,(caar e1))) `(,(string->symbol(string-append "thunk" (number->string num))))) (+ num 1))))))

;Name: getvar
;Arguments: e1 – A set of expressions; e2 – a list
;Description: Used in the let conditional, getvar returns all of the variables within the let. For instance, (let ((x 2) (y 3))) would return the list (x y).
;Precondition: A list of assignments from a let and an empty list.
;Postcondition: A list of all variables from the assignments.
;Cost Analysis: O(n)
(define getvar (lambda (e1 e2) ;e1 = set of sequences
                 (if (or (not (null? e1)))
                     (getvar (cdr e1) (append e2 (list (caar e1))))
                     e2)))

#|
Name: getinit
Arguments: e1 – A set of expressions; e2 – a list
Description: Used in the let conditional, getinit returns all of the initializations within the let.
Precondition: A list of assignments from a let and an empty list.
Postcondition: A list of all initializations from the assignments.
Cost Analysis: O(n)
|#
(define getinit (lambda (e1 e2)
                  (if (not (null? e1))
                      (if (pair? (car (cdar e1))) 
                          (getinit (cdr e1) (append e2 (cons (helper (car (cdar e1)) '()) '())))
                          (getinit (cdr e1) (append e2 (helper (list (car (cdar e1))) '()))))
                      e2)))

#|
Name: getvarrec
Arguments: e1 – A set of expressions; e2 – a list
Description: Used in the letrec conditional, getvarrec returns all of the variables within the letrec and combines them with the term “undefined”.
Precondition: A list of assignments from a letrec and an empty list.
Postcondition: A list of all variables from the assignments, with each variable paired with the symbol “undefined”.
Cost Analysis: O(n)
|#
(define getvarrec (lambda (e1 e2)
                    (if (not (null? e1))
                     (getvarrec (cdr e1) (append e2 (list (cons (caar e1) '(undefined))))) ;could turn this into just (list (caar e1) 'undefined)?
                     e2)))
                       
#|
Name: getinitrec
Arguments: e1 – A set of expressions; e2 – a list; num – the current temp variable number in use
Description: Used in the letrec conditional, getinit returns all of the initializations within the letrec as the tail of a list who’s head the term “temp” concatenated with num to produce a term like “temp1”.
Precondition: A list of assignments from a letrec, an empty list, and a starting number.
Postcondition: A list of all initializations from the assignments as the tail of a list with the head being “temp” + num.
Cost Analysis: O(n)
|#
(define getinitrec (lambda (e1 e2 num)
                     (if (not (null? e1))
                         (if (pair? (car (cdar e1))) 
                          (getinitrec (cdr e1) (append e2 (list `,(string->symbol (string-append "temp" (number->string num))) (cons (helper (list (car (cdar e1))) '()) '()))) (+ num 1))
                          (getinitrec (cdr e1) (append e2 (list `,(string->symbol (string-append "temp" (number->string num))) (helper (list (car (cdar e1))) '()))) (+ num 1)))
                      e2)))

#|
Name: getcombos
Arguments: e1 – A set of expressions; e2 – a list; num – the current temp variable number in use
Description: Used in the letrec conditional, getcombos takes the same set of expressions used in getvarrec and getinitrec to once again attain all of the variables. The difference is that getcombos takes those variables and puts them in the format of (set! Variable temp+num).
Precondition: A list of assignments from a letrec, an empty list, and a starting number.
Postcondition: A list of all variables in the format (set! Var temp+num) in succession.
Cost Analysis: O(n)
|#
(define getcombos (lambda (e1 e2 num) 
                    (if (not (null? e1))
                        (getcombos (cdr e1) (append e2 `(set! ,(caar e1) ,(string->symbol (string-append "temp" (number->string num))))) (+ num 1))
                        e2)))

(letrec
    #|
Name: new-env
Arguments: idl, vals, env
Description: Takes a list of identifiers and their respective values and creates a pairing between them. Once an id is paired with its value in a list, that list is appended to the given environment.
Precondition: Each identifier has a corresponding value
Postcondition: The pairings of ids and vals are added to the environment
Cost Analysis: O(n)
|#
   ((new-env
    (lambda (idl vals env)
                (if (or (null? idl) (null? vals))
                    env
                    (new-env (cdr idl) (cdr vals) (append env (list (list (car idl) (car vals))))))))

#|
Name: lookup
Arguments: id, env
Description: Checks the environment for the given identifier. If it is present, then the identifier's value is returned, otherwise the evaluation of the identifier is returned.
Precondition: N/A
Postcondition: Value of the given identifier
Cost Analysis: O(1)
|#
   (lookup
    (lambda (id env)
                (let ((pair (assq id env)))
                  (if pair (cadr pair) (id)))))
  
#|
Name: assign 
Arguments: id, val, env 
Description: Creates a pairing between the id and value before appending it to the environment.
Precondition: N/A
Postcondition: Value of id is now paired with the id and appended to the environment.
Cost Analysis: O(1)
|#
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
#|
Name: interpret
Arguments: exp – A quoted expression that would work correctly if run normally
Description: The primary function and the one that actually returns the values of the core expressions. As a letrec, Interpret defines four other functions: assign, exec, new-env, and lookup. It uses each of these functions to gather the correct information from the top level environment or add info to the environment if that is what the expanded expression’s purpose is.
Precondition: An expression that can be correctly expanded by expand.
Postcondition: The expression’s evaluation or its procedures.
Cost Analysis: O(1)
|#
(set! interpret
      (lambda (exp)
            (exec (expand exp) top-level-env))))
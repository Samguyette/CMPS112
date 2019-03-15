#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.10 2019-01-15 14:10:54-08 - - $
;;
;; NAME
;; Partner: Sergey Gasparyan (sgaspary@ucsc.edu)
;; Partner: Samuel Guyette (sguyette@ucsc.edu)
;;
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; File i/o
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))


;;Hashtable definition
(define *array-table* (make-hash))
(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *variable-table* (make-hash))
;; Initialize Variable Hashtable
(for-each
   (lambda (entry)
        (hash-set! *variable-table* (car entry) (cadr entry)))
   `(
        (e ,(exp 1.0))
        (pi ,(acos -1.0))
        (nan ,(/ 0.0 0.0))
        (eof  0.0)
   )
)
;; Initialize Function Hashtable
(for-each
   (lambda (entry)
        (hash-set! *function-table* (car entry) (cadr entry)))
   `(
        (abs ,abs)
        (acos ,acos)
        (asin ,asin)
        (atan ,atan)
        (ceil ,ceiling)
        (cos , cos)
        (exp , exp)
        (floor , floor)
        (log , log)
        (log10 , (lambda (a) (/ (log a) (log 10.0))))
        (log2 , (lambda (a) (/ (log a) (log 2.0))))
        (round , round)
        (sin , sin)
        (sqrt , sqrt)
        (tan , tan)
        (trunc , truncate)
        (+ ,+)
        (<= ,<=)
        (< ,<)
        (= ,equal?)
        (<> ,(lambda (a b) (not (= a b))))
        (> ,>)
        (>= ,>=)
        (- ,-)
        (* ,*)
        (/ ,/)
        (^ ,expt)
   )
)

(define *run-file*
    (let-values
        (((dirpath basepath root?)
             (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;; Hashtable get and put functions
(define (function-get key)
   (hash-ref *function-table* key))
(define (function-put! key value)
   (hash-set! *function-table* key value))
(define (variable-get key)
   (hash-ref *variable-table* key))
(define (variable-put! key value)
   (hash-set! *variable-table* key value))
(define (label-get key)
   (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))


;Interpret let function
;
(define (interpret-let variable expression)
    (cond

        ((symbol? variable)
            (hash-set! *variable-table* variable
                (evaluate-expression expression)))

        ((and (pair? variable)(hash-has-key? *array-table*
        (cadr variable))(<= (evaluate-expression(caddr variable))
        (vector-length (hash-ref *array-table* (cadr variable)))))
            (vector-set! (hash-ref *array-table* (cadr variable))
            (- (exact-round (evaluate-expression(caddr variable))) 1)
            (evaluate-expression expression)))
        ((equal? 1 1)
            (die '("Error: Invalid variable or array")))
))

(define (interpret-input memory)
   (let input ((memory memory))
        (when (not(null? memory))
           (let ((in (read)))
              (cond
                 ((number? in)
                     (cond
                         ((symbol? (car memory))
                             (variable-put! (car memory) in)
                             (input(cdr memory)))

                         ((and (pair? (car memory))
                         (hash-has-key? *array-table* (caar memory))
                         (<= (evaluate-expression(cadar memory))
                         (vector-length (car (hash-ref *array-table*
                         (caar memory))))))
                             (vector-set! (hash-ref *array-table*
                             (caar memory)) (- (evaluate-expression
                             (cadar memory)) 1) in)
                                 (input (cdr memory)))))
                ((eof-object? in)
                    (variable-put! `eof 1.0)
                    (variable-put! (car memory) (/ 0.0 0.0)))

                 ((equal? 1 1)
                    (variable-put! (car memory) (/ 0.0 0.0))))))))


;Depending on what statement is inputed the function
;sends important data to the individual interpret function
;fiior the statement
(define (interpret-statement statement)
    (cond

        ((null? statement))

        ((eqv? (car statement) 'print)
            (interpret-print (cdr statement)))

        ((eqv? (car statement) 'if)
            (interpret-if (cadr statement) (caddr statement)))

        ((eqv? (car statement) 'dim)
            (interpret-dim (cadadr statement)
                (car (cddadr statement))))

        ((eqv? (car statement) 'input)
            (interpret-input (cdr statement)))

        ((eqv? (car statement) 'goto)
            (interpret-goto(cadr statement)))

        ((eqv? (car statement) 'let)
            (interpret-let (cadr statement)(caddr statement)))))


;; Checks if conditions is true. If it is calls
;; goto label.
(define (interpret-if condition label)
    (cond

        (((function-get (car condition))
        (evaluate-expression (cadr condition))
        (evaluate-expression (caddr condition)))
            (interpret-goto label))))

(define (evaluate-expression expression)
    (cond
        ((number? expression)
            (+ 0.0 expression))

        ((and (pair? expression)
            (hash-has-key? *function-table* (car expression)))
            (let ((fn (function-get (car expression)))
                 (args (map evaluate-expression (cdr expression))))
                 (apply fn args)))

        ((and (pair? expression) (hash-has-key? *array-table*
        (cadr expression)) (<= (evaluate-expression (caddr expression))
        (vector-length (hash-ref *array-table* (cadr expression)))))
             (vector-ref (hash-ref *array-table* (cadr expression))
             (- (exact-round(evaluate-expression
             (caddr expression))) 1)))

        ((symbol? expression)
             (+ (evaluate-expression(variable-get expression)) 0.0))

        ((equal? 1 1)
             (die '("Error: Expression not recognized.")))))


;; interprets the print statement by checking if it is a
;; string or a list. If it is a string it prints it. If it
;; is a list it calls evaluate expression on it and then prints the
;; result. After this line is fully printed the function moves on
;; to print the rest of the lines.
(define (interpret-print to-be-printed)
    (let printing ((to-be-printed to-be-printed))
         (when (not(null? to-be-printed))
            (if(string? (car to-be-printed))
               (printf "~a" (car to-be-printed))
               (printf " ~a" (evaluate-expression(car to-be-printed)))
            )
         (printing(cdr to-be-printed))))
     (printf "~n"))


;; Interprets the dim statement by adding an array to the
;; array hashtable. The key will be the name of the array
;; and the value will an array of size expression.
(define (interpret-dim name expression)
        (hash-set! *array-table* name (make-vector
        (abs (exact-round(evaluate-expression expression))))))

;; Interprets the goto statement by searching for the label
;; in the label hashtable. If it finds the label
;; interpret program will be called on the line that that
;; label is on. Else it will through an error.
(define (interpret-goto label)
        (if (hash-has-key? *label-table* label)
            (interpret-program (label-get label))
            (die '("Error: goto called with undefined label."))))

;;Finds all of the labels and stores them in the
;;label hashtable with the label as the key and
;;the top level node that points at the line the label
;;is on as the value
;;Checks if length of list is 2 or 3 because these are
;;the only lists that can have labels
(define (create-label-table program)
    (let search-for-label ((program program))
         (when (not (null? program))
             (when (or (equal? (length (car program)) 3)
             (equal? (length (car program)) 2))
                 (when (not (pair? (cadar program)))
                     (label-put! (cadar program) program)))
             (search-for-label (cdr program)))))

;;Finds all of the statements and calls
;;interpret-statement on them
;;If statement is (goto or true if) (exit 0)
;;else continue with original control sequence
;;by recuring on the cdr
(define (interpret-program program)
    (let search-for-statement ((program program))
         (cond
              ((null? program))

              ((equal? (length (car program)) 3)
                  (interpret-statement(caddar program))
                  (when(or (eqv? 'goto (car(caddar program)))
                  (and (eqv? 'if (car(caddar program)))
                  ((function-get (car (cadr(caddar program))))
                  (evaluate-expression (cadr(cadr(caddar program))))
                  (evaluate-expression(caddr(cadr(caddar program)))))))
                      (exit 0))
                  (search-for-statement(cdr program))
                  (exit 0))

              ((and (equal? (length (car program)) 2)
              (pair? (cadar program)))
                  (interpret-statement(cadar program))
                 (when(or (eqv? 'goto (car(cadar program)))
                  (and (eqv? 'if (car(cadar program)))
                  ((function-get (car (cadr(cadar program))))
                  (evaluate-expression (cadr(cadr(cadar program))))
                  (evaluate-expression(caddr(cadr(cadar program)))))))
                     (exit 0))
                  (search-for-statement(cdr program))
                  (exit 0))

              ((or (equal?(length (car program)) 1)
              (equal? (length (car program)) 2))
                  (search-for-statement (cdr program))))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
                (create-label-table program)
                (interpret-program program))))

(main (vector->list (current-command-line-arguments)))

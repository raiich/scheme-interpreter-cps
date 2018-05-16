
(define (eval-set!-cps exp env cont)
  (if (eq? (length exp) 3) ;(set! var val) <-3
   (cont
     (set-variable-value! (cadr exp)
                       (my-eval-cps (caddr exp) env (lambda (x) x))
                       env))
   (display "bad syntax - SET!\n")))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (begin
          (display "Unbound variable in SET!:")(display var)(newline))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))




(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (eval-definition exp env cont)
  (cont (define-variable! (definition-variable exp)
                    (my-eval-cps (definition-value exp) env (lambda (x) x))
;    (definition-value exp)
                    env)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (lookup-variable-value var env cont)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cont (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (begin (display "Unbound variable:")(display var)(newline)(void))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))




;;; extend-env: [Var] * [Val] * Env -> Env
;;;;;my extend-env
(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (display "many arguments.\n")
          (display "few arguments.\n"))))


;;; env-lookup: Var * [Var] * [Val] * Env -> Val
(define (env-lookup x x* v* r)
  (if (null? x*)
      (r x)
      (if (eq? x (car x*))
          (car v*)
          (env-lookup x (cdr x*) (cdr v*) r))))








(define (eval-letrec-cps exp env cont)
  (define (add-let let-list env)
    (if (not (null? let-list))
        (let ((var (caar let-list))
              (val '()))
          (define-variable! var val env)
          (add-let (cdr let-list) env))))
  (define (set-let let-list env)
    (if (not (null? let-list))
        (let ((var (caar let-list))
              (val (my-eval-cps (cadar let-list) env (lambda (x) x))))
          (set-variable-value! var val env)
          (set-let (cdr let-list) env))))
  (let ((newenv (extend-env '() '() env)))
    (add-let (cadr exp) newenv)
    (set-let (cadr exp) newenv)
    (my-eval-cps (caddr exp) newenv cont)))



(define (let-var e) (caar e))
(define (let-val e) (cadar e))
(define (let-var-list letexp)
  (if (eq? letexp '()) '()
      (cons (let-var letexp) (let-var-list (cdr letexp)))))


(define (eval-let-cps******* exp env cont)
  (define (let-val-list letexp)
    (if (eq? letexp '()) '()
        (cons
         (eval-now (let-val letexp))
         (let-val-list (cdr letexp)))))
  (define (eval-now exp)
    (let ((res (my-eval-cps exp env (lambda (x) x))))
      (if (eq? res get-void)
          (set! cont (display ""))
          res)))
  (my-eval-cps 
   (cons (list 'lambda (let-var-list (cadr exp)) (caddr exp)) 
         (let-val-list (cadr exp)))
   env cont))
;  (display
 ; (cons (list 'lambda (let-var-list (cadr exp)) (caddr exp)) (let-val-list (cadr exp))))
  ;)

(define (eval-let-cps exp env cont)
  (define (let-val-list letexp)
    (if (eq? letexp '()) '()
        (cons (let-val letexp) (let-val-list (cdr letexp)))))
  (my-eval-cps 
   (cons (list 'lambda (let-var-list (cadr exp)) (caddr exp)) 
         (let-val-list (cadr exp)))
   env cont))
  



;caadr :first bound "(var,val) pair". cdadr: next pair. cddr: body of exp.
(define (eval-let*-cps exp env cont)
  (if (null? (cadr exp))
      (my-eval-cps (list 'let '() (caddr exp)) env cont)
      (my-eval-cps
       (list 'let (list (caadr exp)) (cons 'let*  (cons (cdadr exp) (cddr exp))))
       env
       cont)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check variables in let if they do not occur the "let: duplicate identifier".
(define (let-check letexp)
  'code)
;構文チェックと文法チェックは最初にする?
;変数チェックをここでする?






(define (eval-do-cps exp env cont)
  (define (var-list do-exp)
    (if (null? do-exp) '()
        (cons (caar do-exp) (var-list (cdr do-exp)))))
  (define (val-list do-exp)
    (if (null? do-exp) '()
        (cons (my-eval-cps (cadar do-exp) env (lambda (x) x))
              (val-list (cdr do-exp)))))
  (define (update-list do-exp newenv)
    (if (null? do-exp) '()
        (cons (my-eval-cps (caddar do-exp) newenv (lambda (x)x))
              (update-list (cdr do-exp) newenv))))
  (define (set!-loop vars vals env)
    (if (not (null? vars))
      (begin
        (set-variable-value!
         (car vars)
         (car vals)
         env)
        (set!-loop (cdr vars) (cdr vals) env))))
  
  (define (do-loop newenv)
    (if (my-eval-cps (caaddr exp) newenv (lambda (x) x))
        (my-eval-cps (cadar (cddr exp)) newenv cont)
        (begin
          (eval-sequence-cps (cdddr exp) newenv (lambda (x) x))
          (set!-loop 
           (var-list (cadr exp))
           (update-list (cadr exp) newenv)
           newenv)
          (do-loop newenv))))
  
  (let ((newenv 
         (extend-env (var-list (cadr exp)) (val-list (cadr exp)) env)))
    (do-loop newenv)))




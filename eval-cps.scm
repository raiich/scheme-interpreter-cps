(load "init-env.scm")
(load "eval-cond-cps.scm")

(define lambda-args cadr)
(define lambda-body cddr)
(define quote-exp cadr)
(define app-fun car)
(define app-args cdr)

(define get-void (set! null '()))

(define (const? e)
  (or (number? e) (boolean? e) (null? e)))

;;; my-eval-cps: Exp * Env * Cont -> Ans
(define (my-eval-cps e r k)
  (cond ((const? e) (k e))
;        ((procedure? e) (k e))
       ((string? e) (k e))
       ((symbol? e)  (lookup-variable-value e r k))
;                                           (lambda (x) (my-eval-cps x r k))))
       ((pair? e)
        (cond ((eq? (car e) 'lambda)
               (k (make-closure-cps (lambda-args e) (lambda-body e) r)))
             ((eq? (car e) 'if) (eval-if-cps e r k))
             ((eq? (car e) 'quote) (k (quote-exp e))) 
             ; (my-eval-cps (quote-exp e) r k))
             ((eq? (car e) 'define)   (eval-definition e r k))
             ((eq? (car e) 'set!) (eval-set!-cps e r k))
             ((eq? (car e) 'begin) (eval-sequence-cps (cdr e) r k))
             ((eq? (car e) 'cond) (eval-cond-cps e r k))
             ((eq? (car e) 'let) (eval-let-cps e r k))
             ((eq? (car e) 'let*) (eval-let*-cps e r k))
             ((eq? (car e) 'letrec) (eval-letrec-cps e r k))
             ((eq? (car e) 'and) (eval-and-cps e r k))
             ((eq? (car e) 'or) (eval-or-cps e r k))
             ((eq? (car e) 'do) (eval-do-cps e r k))
             (else
              (my-eval-cps (app-fun e)
                          r
                          (lambda (v) (my-eval-cps* (app-args e) r (lambda (v*) (v v* k))))))))
                                           ;vはclosure(引数: 実引数, 継続) made by make-closure
       (else
        (display "Invalid expression-mec: ")(display e)(newline))))

;;; my-eval-cps*: [Exp] * Env * Cont -> Ans
(define (my-eval-cps* e* r k)
  (if (null? e*)
      (k ())
;      (if (pair? e*)
      (my-eval-cps (car e*)
                   r
                   (lambda (v)
                     (my-eval-cps* (cdr e*)
                                   r
                                   (lambda (v*)
                                     (k (cons v v*))))))))
;      (my-eval-cps e* r k))))
  


;後で実行されたときに　引数を受け取るのだが、その実行するときに「環境を作り、変数の束縛を行うラムダ式」を返す
;;; make-closure-cps: [Var] * Exp * Env -> Fun
(define (make-closure-cps x* e r)
  (lambda (v* k)
     (eval-sequence-cps e (extend-env x* v* r)  k)))




(define (last-exp? e)
  (null? (cdr e)))

(define (eval-sequence-cps exps env cont)
  (cond ((last-exp? exps)
         (my-eval-cps (car exps) env (lambda (x) (cont x))))
        (else
;         (display exps)
         (my-eval-cps (car exps) env (lambda (x) ;x))
         (eval-sequence-cps (cdr exps) env cont))))))






(define (eval-and-cps exp env cont)
  (eval-early '#f (cdr exp) env cont))

(define (eval-or-cps exp env cont)
  (eval-early '#t (cdr exp) env cont))

(define (eval-early rule exp env cont)
  (if (null? exp)
      (cont (not rule))
    (my-eval-cps
     (car exp)
     env
     (lambda (x)
       (if (eq? x rule) (cont x)
          (eval-early rule (cdr exp) env cont))))))






;;; init-cont: Cont
(define (init-cont v)
  (if (not (void? v))
      (begin
        (display ";> ")
        (display v)
        (newline))))



;shall delete in this file. put this in init-env.scm
(define (my-loop)
  (display ";your input:\n")
  (let ((input (read)))
    (my-eval-cps input global-env-cps init-cont) ;#void
    (my-loop)))

(my-loop)


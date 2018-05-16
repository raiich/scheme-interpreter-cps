(define if-condition cadr)
(define if-then caddr)
;(define if-else cadddr)
(define if-else
  (lambda (x)
    (if (null? (cdddr x))
;        '(display "")
        '(void)
        (cadddr x))))

(define (eval-if-cps e r k)
  (my-eval-cps (if-condition e)
               r
               (lambda (v)
                 (my-eval-cps (if v
                                  (if-then e)
                                  (if-else e))
                              r
                              k))))

(define (eval-cond-cps exp env cont)
  (my-eval-cps (caadr exp)
               env
               (lambda (v)
                 (my-eval-cps (if v
                                  (cadadr exp)
                                  (cons 'cond (cddr exp)))
                              env
                              cont))))

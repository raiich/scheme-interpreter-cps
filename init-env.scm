(load "eval-env.scm")
;(load "myprocs.scm")

(define (neq? a b)
  (not (eq? a b)))

(define (last li)
  (if (pair? li)
      (if (null? (cdr li))
          (car li)
          (last (cdr li)))
      (error "not proper argument - last, whose proper argument is list)")))

(define null ())
(define void (lambda x (display "")))
(define (void? x)
  (eq? (void) x))




;;; call/cc-fun: Fun
;call/ccで与えた　関数を使うなら　継続 k は破棄される
(define call/cc-fun
  (lambda (v* k)
    (cond ((not (eq? (length v*) 1))
           (display "call/cc needs 1 argument.\n"))
          ((not (procedure? (car v*)))
           (display "call/cc needs procedure-type argument.\n"))
          (else
           ((car v*) (list (lambda (w* kk) (k (car w*)))) k)))))




(define (eval-apply v* k)
  (let ((res ((car v*) (cadr v*) (lambda (x) x))))
    (k res)))
;  (display v*))
  
(define (eval-map v* k)
  (k (apply map 
           (cons
            (lambda arg* ((car v*) arg* (lambda (x)x))) ;proc
            (cdr v*)))))  ; arguments list





(define (div v* k)
  (define (loop li)
    (if (null? li) #t
       (if (number? (car li))
          (loop (cdr li))
          #f)))
  (if (exist-zero? (cdr v*))
      (display "## division by zero.\n")
      (if (loop v*)
         (k (apply / v*))
         (display "not proper argument(s).\n"))))

(define (exist-zero? li)
  (if (null? li) #f
      (if (eq? (car li) 0)
          #t
          (exist-zero? (cdr li)))))
      

(define (eval-append v* k)
  (if (null? (cdr v*)) (k (car v*))
     (if (list? (car v*)) (eval-append (cdr v*) (lambda (x) (k (append (car v*) x))))
        (begin (display "arguments are not proper (LIST) - APPEND  given:")(display (car v*))(newline)))))


(define (eval-load-cps v* k)
  (define file-stream (open-input-file (car v*)))
  (define (loop)
    (let ((input (read file-stream)))
      (if (eof-object? input)
        (begin
          (display (car v*))
          (display " LOAD - done.")(newline))
        (begin
;          (display a)(newline)(newline)
;          (my-eval-cps input global-env-cps init-cont)
          (my-eval-cps input global-env-cps (lambda (x) x))
          (loop)))))
  (k (loop)))



(define (eval-car v* k)
  (if (null? (cdr v*))
     (if (pair? (car v*))
        (k (caar v*))
        (display "arguments is not pair - CAR\n"))
     (display "CAR expect 1 argument.\n")))


(define (eval-cdr v* k)
  (if (null? (cdr v*))
     (if (pair? (car v*))
        (k (cdar v*))
        (display "arguments is not pair - CDR\n"))
     (display "CDR expect 1 argument.\n")))








;(define (make-app f) ;; 引数リスト と 継続 を apply で適応させる procedure を作成
;  (lambda (v* k) (k (apply f v*))))
  
(define (make-app f) ;; 引数リスト と 継続 を apply で適応させる procedure を作成
  (define (number?-loop v*)
    (if (null? v*) #t
       (if (number? (car v*)) (number?-loop (cdr v*))
          #f)))
  (define (req x) (display "req undefined.\n"))
  (set! req 
    (cond
;    ((or (eq? f +) (eq? f -) (eq? f *) (eq? f /)) 
        ((memq f (list + - * /)) 
         (lambda (v*) (and (>= (length v*) 1) (number?-loop v*))))
        ((memq f (list = > >= < <=)) 
         (lambda (v*) (and (>= (length v*) 2) (number?-loop v*))))
        ((memq f (list null? pair? list? symbol? number? string? boolean? procedure? not))
          (lambda (v*) (= (length v*) 1)))
        ((memq f(list cons eq? neq? equal?)) 
         (lambda (v*) (= (length v*) 2)))
        ((memq f (list set-car! set-cdr!)) 
         (lambda (v*) (and (= (length v*) 2) (pair? (car v*)))))
        ((memq f (list memq)) 
         (lambda (v*) (and (= (length v*) 2) (list? (cadr v*)))))
        ((memq f (list last length))
         (lambda (v*) (and (= (length v*) 1) (list? v*))))
        ((memq f (list string-append))
         (lambda (v*) (map string? v*)))
        (else  (lambda (v*) #t))))

  (lambda (v* k) 
    (if (req v*)
       (k (apply f v*))
       (begin (display "not proper argument(s) in ")(display f)(newline)))))








;;; empty-env-cps: Env
(define empty-env-cps '())

;;; global-env-cps: Env
(define global-env-cps
  (extend-env
   '(
     call/cc ; arty 1:proc
     call-with-current-continuation
     /     ;; arty >= 1 & num
     apply ;arty 2 :proc, list
     map ;arty >=2 :proc, list+ 
     append ; arty >=0 : list
     car cdr ;arty 1: pair
     load ;arty 1

     + - * ;; arty >= 1 & num
     = > >= < <= ;; arty >= 2  & num
     not ;arty 1
     null? pair? list? symbol? number? string? boolean? procedure? ;arty 1
     cons eq? neq? equal? ;arty 2

     list ;arbitary arty and arg(s)
     set-car! set-cdr! ;arty 2: pair, obj
     memq ; arty 2: obj, list
     last length ;arty 1: list

     string-append ;
     symbol->string string->symbol string->number number->string

;; appendix
     null else 
;; appendix proc
     caar caaar caaaar caaadr caadr cdaaar cdaadr
     cadr cdaar cadaar cadadr cdadr cddaar cddadr
     cdar cadar caadar caaddr caddr cdadar cdaddr
     cddr cddar caddar cadddr cdddr cdddar cddddr
     void?
     display newline　write-char read void eof-object?
     open-input-file
    ) ;;end


   (append ;;begin
    (list
     call/cc-fun call/cc-fun div eval-apply eval-map eval-append
     eval-car eval-cdr ;eval-set-car! eval-set-cdr!
     eval-load-cps)
    
    (map make-app (list 
;    (map make-4op (list
     + - * ;; arty >= 1 & num
;     )
     = > >= < <= ;; arty >= 2  & num
     not ;arty 1

;     (map make-app (list 

     null? pair? list? symbol? number? string? boolean? procedure? ;arty 1
     cons eq? neq? equal? ;arty 2

     list ;arbitary arty, arg
     set-car! set-cdr! ;arty 2: pair, obj
     memq ; arty 2: obj, list
     last ;arty 1:list (pair)?
     length ;arty 1: list
     
     string-append
     symbol->string string->symbol string->number number->string
     ))
    
;; appendix
    (list null #t )
;; appendix proc
    (map make-app (list
     caar caaar caaaar caaadr caadr cdaaar cdaadr
     cadr cdaar cadaar cadadr cdadr cddaar cddadr
     cdar cadar caadar caaddr caddr cdadar cdaddr
     cddr cddar caddar cadddr cdddr cdddar cddddr
     void?
     display newline　write-char read void eof-object?
     open-input-file
     )))
   '()))

  




(define (g-env)
  (define env global-env-cps)
;  (define (de-iter env)
;    (if ()))
  (define (de-iter var-list val-list)
    (display (car var-list))(display " : ")
    (display (car val-list))(newline)
    (de-iter (cdr var-list) (cdr val-list)))
  (de-iter (caar env) (cdar env)))
#!/usr/bin/guile -s
!#

;;; ALGORITHMIC LANGUAGE ZJEB (a prototype)
;;; -- def is syntactic, not semantic (being evaluated every time)
;;; -- therefore "(def s ...s...)" means "s" stands for an infinite expression
;;; -- lexical scope with the exception of definitions
;;; -- no more lambdas, because they were invented by church, and religions suck
;;; -- no more car, cdr and cons -- gonna miss them...
;;; -- quasiquote and dotted notations are the same as in scheme

(use-modules (srfi srfi-1)
	     (ice-9 nice-9)
	     #;(ice-9 pretty-print))

(define (primitive? s) (member s '(+ - * < = atom? num?)))

(define (lookup s env)
  (match env
    [() #f]
    [((s0 . v0) . env0) (if (eq? s s0) v0 (lookup s env0))]))

(define (update s v env) `[(,s . ,v) . ,(alist-delete s env)])

(define (bind xs ys)
  (call/cc (lambda (kuurwa)
	     (let ((env
		    (let bnd ((xs xs) (ys ys))
		      (match `(,xs ,ys)
			[(() ()) '()]
			[((? number? n) (? number? n)) '()]
			[((? number? n) _) (kuurwa #f)]
			[(('quote e) e) '()]
			[(('quote e) _) (kuurwa #f)]
			[('_ _) '()]
			[((? symbol? s) e) `((,s . ,e))]			
			[((h1 . t1) (h2 . t2))
			 (append (bnd h1 h2) (bnd t1 t2))]
			[_ (kuurwa #f)]))))
	       (let check-n-clean ((env env))
		 (match env
		   [() '()]
		   [((s . v) . env0)
		    (let ((v0 (lookup s env0)))
		      (cond [(not v0)
			     `((,s . ,v) . ,(check-n-clean env0))]
			    [(equal? v0 v)
			     (check-n-clean env0)]
			    [else (kuurwa #f)]))]))))))

;;; unit test jak ta lala:
;(bind '((q w) q) '((3 2) 3))
;(bind '(x . x) '((1 2) 1 2))
;(bind 'x 3)
;(bind '((q w) q) '((3 2) 1))
;(bind 1 1)
;(bind '('if a b c) '(if elo du pa))
;(bind '('if a b c) '(iff elo du pa))
;(bind '(_ x) '(elo 12))

;;; hahaha!
(define (and->ifs es) (fold-right (lambda (h t) `(if ,h ,t ())) 'T es))
(define (or->ifs es)  (fold-right (lambda (h t) `(if ,h T ,t)) '() es))
;(and->ifs '(a b c))
;(or->ifs '(a b c))

(define (evaluate expr env topenv)
  (let evl ((expr expr))
    (match expr
      [() '()]
      ['T 'T]
      [(? number? n) n]
      [(? primitive? p) p]
      [('&bind cases env) expr]      
      [(? symbol? s)
       (match (lookup s env)
	 [#f (match (lookup s topenv)
	       [#f (error `(unbound symbol ,s) topenv)]
	       [e (evl e)])] ;; note the evl!
	 [e e])]
      [('bind . cases) `(&bind ,cases ,env)]      
      [('if p c a) (if (null? (evl p)) (evl a) (evl c))]
      [('and . es) (evl (and->ifs es))]
      [('or . es) (evl (or->ifs es))]
      [('not e) (evl `(if ,e () T))] ;; i co mi zrobisz?
      [('quote e) e]
      [('quasiquote e)
       (let evqq ((expr e))
	 (match expr
	   [() '()]
	   [('unquote e) (evl e)]
	   [(h . t) `(,(evqq h) . ,(evqq t))]
	   [e e]))]
      [app (match (map evl app)
	     [('+ (? number? n1) (? number? n2)) (+ n1 n2)]
	     [('- (? number? n1) (? number? n2)) (- n1 n2)]
	     [('* (? number? n1) (? number? n2)) (* n1 n2)]	     
	     [('< (? number? n1) (? number? n2)) (if (< n1 n2) 'T '())]
	     [('= e e) 'T]
	     [('= _ _) '()]
	     [('atom? (? pair?)) '()]
	     [('atom? _) 'T]
	     [('num? (? number?)) 'T]
	     [('num? _) '()]
	     [(('&bind cases benv) . vals)
	      (let try-binding ((cases cases))
		(match cases
		  [() (error `(no appropriate binding in ,app) topenv)]
		  [(args body . remaining)
		   (let ((maybe-binding (bind args vals)))
		     (if maybe-binding
			 (evaluate body (append maybe-binding benv) topenv)
			 (try-binding remaining)))]))]
	     [_ (error `(application error ,app) topenv)])])))

;(evaluate '([bind (x) (* x x)] (+ 2 3)) '() '())
#;(evaluate '(f (+ 2 3))
	  '()
	  `[(f . ,(evaluate '[bind (0) 1 (n) (* n (f (- n 1)))] '[] '[] ))])
    
(define (error msg topenv) (repl msg topenv)) ; :D

(define (repl out topenv)
  (display out)
  (newline)
  (display '>)
  (match (read)
    [('def s e) (repl `(new shorthand ,s memoized) (update s e topenv))]
    [('halt) (display "Auf Wiedersehen!") (newline) (exit)]
    [('show-topenv) (display-topenv topenv) (repl `(so now you know.) topenv)]
    [e (repl (evaluate e '() topenv) topenv)]))

(define (display-topenv topenv)
  (match topenv
    [() 'akuku]
    [((s . e) . remaining)
     (display s) (display " <- ") (display e) (newline)
     (display-topenv remaining)]))

;; RUN
(begin
  (display "(-- ALGORITHMIC LANGUAGE ZJEB v0.1 --)") (newline)
  (display " copyleft 2016/08/08 by Scislav Dercz ") (newline)
  (display "type (halt) to quit") (newline)
  (newline)
  (repl 'READY. '()))

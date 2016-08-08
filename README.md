# ALGORITHMIC LANGUAGE ZJEB

A lexically-scoped, one-night, anti-religious lisp dialect. Because why not?


### Requirements:

Two things are required -- guile2.0, and (ice-9 nice-9) module. On debian-alikes this might do:
```
apt-get install guile-2.0
wget https://bitbucket.org/panicz/slayer/raw/d8bd36dadaf1246673979bede2c8548c4c53ba77/guile-modules/ice-9/nice-9.scm
mv nice-9.scm <your guile's site path>/ice-9/
```

### Installation:
```
chmod +x proto-zjeb.scm
```
_Et voilà!_


### Example session:

```
(-- ALGORITHMIC LANGUAGE ZJEB v0.1 --)
 copyleft 2016/08/08 by Scislav Dercz 
type (halt) to quit

READY.
>(+ 3 (* 7 8))
59
>`(quasiquotes work ,(+ 2 3))
(quasiquotes work 5)
>(bind (x) (* x x))
(&bind ((x) (* x x)) ())
>((bind (x) (* x x)) (+ 2 3))
25
>(def fac
      (bind (0) 1
            (n) (* n (fac (- n 1)))))
(new shorthand fac memoized)
>(fac (+ 2 3))
120
>(def map
      (bind (f ()) ()
            (f (x . xs)) `(,(f x) . ,(map f xs))))
(new shorthand map memoized)
>(map fac '(1 2 3 4 5))
(1 2 6 24 120)
>(show-topenv)
map <- (bind (f ()) () (f (x . xs)) (quasiquote ((unquote (f x)) unquote (map f xs))))
fac <- (bind (0) 1 (n) (* n (fac (- n 1))))
(so now you know.)
>(def mk-adder (bind (x) (bind (y) (+ x y))))
(new shorthand mk-adder memoized)
>(mk-adder 3)
(&bind ((y) (+ x y)) ((x . 3)))
>((mk-adder 3) 2)
5
>(halt)
Auf Wiedersehen!
```

### Some example programs:

  * lisp.zjeb -- a very crude lisp variant in 39 lines of code,
  * group-theory.zjeb -- now this is neat, definitely check it out -- a bit of abstract algebra in ZJEB!



;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Please read the Copyright notice below.
;;;;
;;;; Implements a Symbolic Simplifier for Algebraic Expressins.
;;;; e.g.
;;;;       (x^2 - 1)/(x - 1) => (x - 1)
;;;;       x + x - 4         => 2 * x - 4

(in-package :gecodecl)
;;;; Code partly from Paradigms of AI Programming (Chapter 15)
;;;; Copyright (c) 1991 Peter Norvig
;;;; File cmacsyma.lisp: Canonical Form version of Macsyma.
;;;; Bug Fix by dst, Dave_Touretzky@CS.CMU.EDU
;;;;
;;;; Portions adapted from Richard J. Fateman's Mock-Mma which bears
;;;; the following Copyright notice:
;;;;
;;;; Copyright (c) 1990-1992 Richard J. Fateman
;;;; Copyright (c) 1990-1992 The Regents of the University of California.
;;;; All rights reserved.
;;;; 
;;;; Permission to use, copy, modify, and distribute this software and its
;;;; documentation for any purpose, without fee, and without written agreement is
;;;; hereby granted, provided that the above copyright notice and the following
;;;; two paragraphs appear in all copies of this software.
;;;; 
;;;; IN NO EVENT SHALL RICHARD J. FATEMAN OR THE UNIVERSITY OF CALIFORNIA
;;;; BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR
;;;; CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
;;;; DOCUMENTATION, EVEN IF RICHARD J. FATEMAN AND THE UNIVERSITY OF
;;;; CALIFORNIA HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; 
;;;; RICHARD J. FATEMAN AND THE UNIVERSITY OF CALIFORNIA SPECIFICALLY
;;;; DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
;;;; THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND RICHARD J.
;;;; FATEMAN AND THE UNIVERSITY OF CALIFORNIA HAVE NO OBLIGATION TO PROVIDE
;;;; MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

;;; A polynomial is either a number (integer -- for now)
;;; or a vector of length d+2 where d>0 is the degree of S
;;; S=#(mainvar coeff coeff ... coeff).
;;;              ^                ^
;;;              |                |-coeff of mainvar^d : non-zero
;;;              |-coeff of mainvar^0
;;; e.g.
;;; 4 + 5*x + x^2  = #(mainvar 4 5 1), where mainvar is a subtype
;;; of gvariable

;;; Coeffs can be polynomials in other variables.

;;; -------------------------------------------------------------------------------

;;; Tool Functions

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defstruct (expr (:type list)
		 (:constructor mkexpr (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(define-condition non-integer-result () ())

(defun var-p (x)
  (typep x 'gvariable))

;;;; Definition of the Canonical Polynomial Representation

(declaim (inline main-var degree coef var-p var= var>
                 poly make-poly exp-p exp-args))

;; two variables are equal if they are of the same type and their
;; indices are equal
(defgeneric var= (x y))
(defmethod var= (x y) nil)
(defmethod var= ((x intvar) (y intvar))
  (= (gvariable-index x) (gvariable-index y)))
(defmethod var= ((x boolvar) (y boolvar))
  (= (gvariable-index x) (gvariable-index y)))
(defmethod var= ((x floatvar) (y floatvar))
  (= (gvariable-index x) (gvariable-index y)))

;; variable order is float > int > bool
;; variables of the same type are ordered by their index
(defgeneric var> (x y))
(defmethod var> (x y) nil)
(defmethod var> ((x intvar) (y boolvar)) t)
(defmethod var> ((x floatvar) (y intvar)) t)
(defmethod var> ((x intvar) (y intvar))
  (> (gvariable-index x) (gvariable-index y)))
(defmethod var> ((x boolvar) (y boolvar))
  (> (gvariable-index x) (gvariable-index y)))
(defmethod var> ((x floatvar) (y floatvar))
  (> (gvariable-index x) (gvariable-index y)))


(deftype polynomial () 'simple-vector)

(defun polynomial-p (p)
  (typep p 'polynomial))

(defsetf main-var (p) (val) 
  `(setf (svref (the polynomial ,p) 0) ,val))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) (+ ,i 1)) ,val))

(defmacro leading-coef (x) `(svref ,x (1- (length ,x))))

(defun main-var (p) (svref (the polynomial p) 0))
(defun coef (p i)   (svref (the polynomial p) (+ i 1)))
(defun degree (p)   (- (length (the polynomial p)) 2))

(defun poly (x &rest coefs)
  "Make a polynomial with main variable x 
  and coefficients in increasing order."
  (apply #'vector x coefs))

(defun make-poly (x degree)
  "Make the polynomial 0 + 0*x + 0*x^2 + ... 0*x^degree"
  (let ((p (make-array (+ degree 2) :initial-element 0)))
    (setf (main-var p) x)
    p))

(defmacro def-function-mapping (list)
  (loop for item in list
        collect `(:method ((x (eql ',(first item)))) #',(second item))
          into methods
        finally (return
                  `(defgeneric get-fn (x)
                     ,@methods))))

(def-function-mapping
    ((+ poly+) (- poly-) (* poly*)
     (^ poly^n) (/ poly/) (D deriv-poly)))

(defun prefix->canon (x)
  "Convert a prefix Lisp expression to canonical form.
  Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
       (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0"
  (cond ((numberp x) x)
        ((var-p x) (poly x 0 1))
        ((exp-p x)
         (apply (get-fn (expr-op x))
                (mapcar #'prefix->canon (exp-args x))))
        (t (error "Not a polynomial: ~a" x))))


;;;; Implementation of the mathmatical operations on the canonical
;;;; representation

(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (case (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))
    (otherwise 
     (let ((poly (poly+poly (first args) (second args))))
       (dolist (p (nthcdr 2 args) poly)
         (setf poly (poly+poly poly p)))))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (case (length args)
    (0 0)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly (first args) (k*poly -1 (second args))))
    (otherwise 
     (let ((poly (poly+poly (first args) (k*poly -1 (second args)))))
       (dolist (p (nthcdr 2 args) poly)
         (setf poly (poly+poly poly (poly*poly -1 p))))))))

(defun poly* (&rest args)
  "Polynomial multiplication where (* P) simply returns P as with
CL::* for numbers."
  (case (length args)
    (1 (first args))
    (2 (poly*poly (first args) (second args)))
    (otherwise 
     (let ((poly (poly*poly (first args) (second args))))
       (dolist (p (nthcdr 2 args) poly)
         (setf poly (poly*poly poly p)))))))

(defun poly/ (&rest args)
  "Polynomial division where (/ P) returns (/ 1 P) as with
CL::/ for numbers."
  (case (length args)
    (1 (poly/poly 1 (first args)))
    (2 (poly/poly (first args) (second args)))
    (otherwise 
     (let ((poly (poly/poly (first args) (second args))))
       (dolist (p (nthcdr 2 args) poly)
         (setf poly (poly/poly poly p)))))))

(defun poly= (p q)
  (cond
    ((and (numberp p) (numberp q)) (= p q))
    ((and (rational-p p) (rational q))
     (and (poly= (rat-numerator p) (rat-numerator q))
          (poly= (rat-denominator p) (rat-denominator q))))
    ((and (polynomial-p p) (polynomial-p q))
     (and (var= (main-var p) (main-var q))
          (= (degree p) (degree q))
          (loop for i from (degree p) downto 0
                always (poly= (coef p i) (coef q i)))))
    (t nil)))

(defun poly-poly (p q)
  (poly+poly p (poly*poly -1 q)))

(defun poly+poly (p q)
  "Add two polynomials."
  (normalize-poly
    (cond
      ((numberp p)                      (k+poly p q))
      ((numberp q)                      (k+poly q p))
      ((or (rational-p p)
           (rational-p q))              (rat+rat p q))
      ((var= (main-var p) (main-var q)) (poly+same p q))
      ((var> (main-var q) (main-var p)) (k+poly q p))
      (t                                (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant k to a polynomial p."
  (cond ((eql k 0) p)                 ;; 0 + p = p
        ((and (numberp k) (numberp p))
         (+ k p))                     ;; Add numbers
        (t (let ((r (copy-poly p)))   ;; Add k to x^0 term of p
             (setf (coef r 0) (poly+poly (coef r 0) k))
             r))))

(defun poly+same (p q)
  "Add two polynomials with the same main variable."
  ;; First assure that q is the higher degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;; Add each element of p into r (which is a copy of q).
      (let ((r (copy-poly q)))
        (loop for i from 0 to (degree p) do
              (setf (coef r i) (poly+poly (coef r i) (coef p i))))
        r)))

(defun copy-poly (p)
  "Make a copy of a polynomial."
  (copy-seq p))

(defun poly*poly (p q)
  "Multiply two polynomials."
  (normalize-poly
    (cond
      ((or (rational-p p)
           (rational-p q))              (rat*rat p q))
      ((numberp p)                      (k*poly p q))
      ((numberp q)                      (k*poly q p))
      ((var= (main-var p) (main-var q)) (poly*same p q))
      ((var> (main-var q) (main-var p)) (k*poly q p))
      (t                                (k*poly p q)))))

(defun k*poly (k p)
  "Multiply a polynomial p by a constant factor k."
  (cond
    ((eql k 0)         0)       ;; 0 * p = 0
    ((eql k 1)         p)       ;; 1 * p = p
    ((and (numberp k)
          (numberp p)) (* k p)) ;; Multiply numbers
    ((rational-p p) (rat*rat k p))
    (t ;; Multiply each coefficient
     (let ((r (make-poly (main-var p) (degree p))))
       ;; Accumulate result in r;  r[i] = k*p[i]
       (loop for i from 0 to (degree p) do
             (setf (coef r i) (poly*poly k (coef p i))))
       r))))

(defun poly*same (p q)
  "Multiply two polynomials with the same variable."
  ;; r[i] = p[0]*q[i] + p[1]*q[i-1] + ...
  (let* ((r-degree (+ (degree p) (degree q)))
         (r (make-poly (main-var p) r-degree)))
    (loop for i from 0 to (degree p) do
          (unless (eql (coef p i) 0)
            (loop for j from 0 to (degree q) do
                  (setf (coef r (+ i j))
                        (poly+poly (coef r (+ i j))
                                   (poly*poly (coef p i)
                                              (coef q j)))))))
    r))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros."
  (cond 
    ((numberp p) p)
    ((rational-p p) (normalize-rat p))
    (t
     (let ((p-degree (position 0 p :test (complement #'eql) :from-end t)))
       (cond ((<= p-degree 1) (normalize-poly (coef p 0)))
             ((<= p-degree (degree p))
              (delete 0 p :start p-degree))
             (t p))))))


(defun deriv-poly (p x)
  "Return the derivative, dp/dx, of the polynomial p."
  ;; If p is a number or a polynomial with main-var > x,
  ;; then p is free of x, and the derivative is zero;
  ;; otherwise do real work.
  ;; But first, make sure X is a simple variable,
  ;; of the form #(X 0 1).
  (assert (and (typep x 'polynomial) (= (degree x) 1)
	       (eql (coef x 0) 0) (eql (coef x 1) 1)))
  (cond
    ((numberp p) 0)
    ((var> (main-var p) (main-var x)) 0)
    ((var= (main-var p) (main-var x))
     ;; d(a + bx + cx^2 + dx^3)/dx = b + 2cx + 3dx^2
     ;; So, shift the sequence p over by 1, then
     ;; put x back in, and multiply by the exponents
     (let ((r (subseq p 1)))
       (setf (main-var r) (main-var x))
       (loop for i from 1 to (degree r) do
             (setf (coef r i) (poly*poly (+ i 1) (coef r i))))
       (normalize-poly r)))
    (t ;; Otherwise some coefficient may contain x.  Ex:
     ;; d(z + 3x + 3zx^2 + z^2x^3)/dz
     ;; = 1 +  0 +  3x^2 +  2zx^3
     ;; So copy p, and differentiate the coefficients.
     (let ((r (copy-poly p)))
       (loop for i from 0 to (degree p) do
             (setf (coef r i) (deriv-poly (coef r i) x)))
       (normalize-poly r)))))

(defun canon->infix (exp)
  (prefix->infix (canon->prefix exp)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions.
Handles operators with any number of args."
  (if (atom exp)
      exp
      (intersperse
        (expr-op exp)
        (mapcar #'prefix->infix (exp-args exp)))))

(defun intersperse (op args)
  "Place op between each element of args.
  Ex: (intersperse '+ '(a b c)) => '(a + b + c)"
  (if (length=1 args)
      (case op
        (- (list op (first args)))
        (/ (list op (first args)))
        (otherwise  (first args)))
      (rest (loop for arg in args
               collect op
               collect arg))))


(defun canon->prefix (p)
  "Convert a canonical polynomial to a lisp expression."
  (cond
    ((numberp p) p)
    ((rational-p p) `(/ ,(canon->prefix (rat-numerator p))
                        ,(canon->prefix (rat-denominator p))))
    (t (let* ((gcd (polynomial-content p))
              (p (if (eql gcd 1)
                     p
                     (poly/poly p gcd))))
         (args->prefix
            '* 1
            (list (canon->prefix gcd) 
                  (args->prefix
                   '+ 0
                   (loop for i from (degree p) downto 0
                         collect (args->prefix
                                      '* 1
                                         (list (exponent->prefix (main-var p) i)
                                               (canon->prefix (coef p i))))))))))))

(defun exponent->prefix (base exponent)
  "Convert canonical base^exponent to prefix form."
  (case exponent
    (0 1)
    (1 base)
    (t `(^ ,base ,exponent))))

(defun args->prefix (op identity args)
  "Convert arg1 op arg2 op ... to prefix form."
  (let ((args (remove identity args)))
    (cond ((null args) identity)
          ((and (eq op '*) (member 0 args)) 0)
          ((length=1 args) (first args))
          (t (cons op  (sort (mappend
                               #'(lambda (exp)
                                   (if (starts-with exp op)
                                       (exp-args exp)
                                       (list exp)))
                               args)
                             #'arg-list-order))))))
(defun arg-list-order (x y)
  (cond
    ((and (listp x)
          (listp y)
          (eql (car x) '^)
          (eql (car y) '^))
     (< (caddr x) (caddr y)))
    #+(or)
    ((and (var-p x)
          (var-p y))
     (var> y x))
    ((and (listp x)
          (var-p y))
     t)
    (t nil)))

(defun canon (exp)
  "Canonicalize argument in prefix form and convert it back to prefix."
  (canon->prefix (prefix->canon exp)))

(defun poly^n (p n)
  "Raise polynomial p to the nth power, n>=0."
  ;; Uses the binomial theorem
  (check-type n (integer 0 *))
  (cond
    ((= n 0) 1)
    ((integerp p) (expt p n))
    (t ;; First: split the polynomial p = a + b, where
     ;; a = k*x^d and b is the rest of p
     (let ((a (make-poly (main-var p) (degree p)))
           (b (normalize-poly (subseq p 0 (- (length p) 1))))
           ;; Allocate arrays of powers of a and b:
           (a^n (make-array (+ n 1)))
           (b^n (make-array (+ n 1)))
           ;; Initialize the result:
           (result (make-poly (main-var p) (* (degree p) n))))
       (setf (coef a (degree p)) (coef p (degree p)))
       ;; Second: Compute powers of a^i and b^i for i up to n
       (setf (aref a^n 0) 1)
       (setf (aref b^n 0) 1)
       (loop for i from 1 to n do
             (setf (aref a^n i) (poly*poly a (aref a^n (- i 1))))
             (setf (aref b^n i) (poly*poly b (aref b^n (- i 1)))))
       ;; Third: add the products into the result,
       ;; so that result[i] = (n choose i) * a^i * b^(n-i)
       (let ((c 1)) ;; c helps compute (n choose i) incrementally
         (loop for i from 0 to n do
               (p-add-into! result c
                            (poly*poly (aref a^n i)
                                 (aref b^n (- n i))))
               (setf c (/ (* c (- n i)) (+ i 1)))))
       (normalize-poly result)))))

(defun p-add-into! (result c p)
  "Destructively add c*p into result."
  (if (or (numberp p)
          (not (var= (main-var p) (main-var result))))
      (setf (coef result 0)
            (poly+poly (coef result 0) (poly*poly c p)))
      (loop for i from 0 to (degree p) do
            (setf (coef result i)
                  (poly+poly (coef result i) (poly*poly c (coef p i))))))
  result)

(defstruct (rat (:constructor make-rat (numer denom)))
  (numer 0  :type (or integer polynomial))
  (denom 1 :type (or integer polynomial)))

(defun rational-p (poly)
  (typep poly 'rat))

(defun normalize-rat (rat)
  (cond
    ((not (eql 'rat (type-of rat)))
     rat)
    ((and (numberp (rat-numer rat))
          (numberp (rat-denom rat)))
     (/ (rat-numer rat) (rat-denom rat)))
    ((numberp (rat-denom rat))
     (let ((p (if (minusp (rat-denom rat))
                  (k*poly -1 (rat-numer rat))
                  (rat-numer rat)))
           (q (abs (rat-denom rat))))
       (if (loop for i fixnum from (degree p) downto 0
                 always (eql 0 (poly-remainder (coef p i) q)))
           (k*poly (/ q) p)
           (make-rat p q)))) ; handle correction of negativ denoms here?!
    (t
     (if (eql 1 (rat-denominator rat))
          (rat-numerator rat)
         rat))))

(defun rat-numerator (rat)
  "The numerator of a rational expression."
  (etypecase rat
    (rat (rat-numer rat))
    (number (numerator rat))))

(defun rat-denominator (rat)
  "The denominator of a rational expression."
  (etypecase rat
    (rat (rat-denom rat))
    (number (denominator rat))))

(defun rat*rat (x y)
  "Multiply rationals: a/b * c/d = a*c/b*d"
  (poly/poly (poly*poly (rat-numerator x)
                        (rat-numerator y))
             (poly*poly (rat-denominator x)
                        (rat-denominator y))))

(defun rat+rat (x y)
  "Add rationals: a/b + c/d = (a*d + c*b)/b*d"
  (let ((a (rat-numerator x))
        (b (rat-denominator x))
        (c (rat-numerator y))
        (d (rat-denominator y)))
    (poly/poly (poly+poly (poly*poly a d) (poly*poly c b))
               (poly*poly b d))))

(defun rat/rat (x y)
  "Divide rationals: a/b / c/d = a*d/b*c"
  (rat*rat x (make-rat (rat-denominator y) (rat-numerator y))))

(defun poly-remainder (p s)
  "Returns the remainder, when p is divided by s. Needed for poly-gcd."
  (cond ((eql 0 s) (error "Division by zero!"))
	((eql 0 p) p)
	((numberp p)
	 (cond ((numberp s) (mod p s))
	       (t p)))
	((or (numberp s)
	     (var> (main-var p) (main-var s))) 0)
	((var> (main-var s) (main-var p)) p)
	(t (poly-remainder-help p s))))

(defun poly-remainder-help (p s)
  "Returns the remainder, when p is divided by s. p and s must have
the same main variable!"
  (if (> (length s) (length p))
      p
      (let* ((s-leading-coef (leading-coef s))
	     (l (- (length p) (length s)))
	     (temp (make-array (+ 2 l) :initial-element 0)))
	(setf (main-var temp) (main-var p))
	(do ((k l m)
	     (m 0))
	    (nil)
	  (setf (leading-coef temp) (leading-coef p)
                temp (delete-if #'identity temp :start (+ 2 k))
                p (poly-poly (poly*poly p s-leading-coef)
                             (poly*poly temp s)))
	  (when (or (numberp p)
                    (var> (main-var p) (main-var s))
                    (< (setf m (- (length p) (length s))) 0))
            (return (if (zerop k)
                        p
                        (poly*poly p (poly^n s-leading-coef k)))))
	  (when (> (1- k) m)
            (setf p (poly*poly p (poly^n s-leading-coef (- (1- k) m)))))))))

(defun poly-gcd (p q)
  "Return the greatest common divisor of two polynoms p q"
  (cond ((eql 0 p) q)
	((eql 0 q) p)
	((numberp p)
	 (cond ((numberp q) (gcd p q))
	       (t (polynomial-content-gcd q p))))
	((numberp q) (polynomial-content-gcd p q))
	((var> (main-var q) (main-var p)) (polynomial-content-gcd p q)) ;!! switch p,q?
	((var> (main-var p) (main-var q)) (polynomial-content-gcd q p))
	(t (polynomial-gcd-same-main-var p q))))

(defun polynomial-gcd-same-main-var (u v)
  "Return GCD of p and q, which _must_ have the same main variable."
  (when (> (length v) (length u)) (rotatef u v))
  (let ((ucont (polynomial-content u))
	(vcont (polynomial-content v)))
    (do ((c (poly/poly u ucont) d)
	 (d (poly/poly v vcont) 
           (let ((rem (poly-remainder c d)))
             (cond ((or (eql 0 rem) (numberp rem)) rem)
                   (t (poly/poly rem (polynomial-content rem)))))))
	(nil)
      (when (eql 0 d)
	(return (poly*poly (poly-gcd ucont vcont) c)))
      (when (or (numberp d) (var> (main-var c) (main-var d)))
	(return (poly-gcd ucont vcont))))))

(defun polynomial-content-gcd (p u)
  "Return the GCD of u and the content of p"
  (do ((i (1- (length p)) (1- i))
       (g u (poly-gcd g (svref p i))))
      ((or (eql 1 g) (= i 0)) g)))

(defun polynomial-content (p)
  "Returns the content of a polynomial: this is the GCD of the coefficients
of the toplevel in the polynomial p."
  (cond ((eql 0 p) p)
	((numberp p) 1)
	(t (let ((len (length p)))
	     (do ((i 2 (1+ i))
		  (g (svref p 1)
		     (poly-gcd g (svref p i))))
		 ((or (= i len) (eql 1 g)) g))))))

(defun poly/poly (p q)
  "Divide p by q: if d is the greatest common divisor of p and q
then p/q = (p/d) / (q/d). Note if q=1, then p/q = p."
  (if (numberp q)
      (poly/k p q)
      (handler-case 
        (let ((d (poly-gcd p q)))
          (normalize-rat
           (make-rat (poly/poly-helper p d)
                     (poly/poly-helper q d))))
        (non-integer-result ()
          (normalize-rat (make-rat p q))))))

(defun poly/poly-helper (p s)
  "Switches to the right division funktion, depending on argument
types."
  (cond ((eql 0 s) (error "Division by zero!"))
	((eql 1 s) p)
	((eql 0 p) p)
	((numberp p)
	 (cond ((numberp s) 
                (if (zerop (mod p s))
                    (/ p s)
                    (signal 'non-integer-result))) 
	       (t (make-rat p s); (error "Division by a polynomial of higher degree!")
                )))
	((numberp s)
	 (poly/k p s))
	(t
         (poly/poly-do-it p s))))

(defun poly/k (p s)
  "Divides a polynomial by a scalar."
  (if (= s 1)
      p
      (handler-case (let ((q (copy-seq p)))
                      (do ((i (1- (length p))
                              (1- i)))
                          ((= i 0) q)
                        (setf (svref q i) (poly/poly-helper (svref q i) s))))
        (non-integer-result ()
          (make-rat p s)))))

(defun poly/poly-do-it (p s)
  "Divides two polynomials p and s."
  (let ((l1 (length p))
	(l2 (length s)))
    (if (> l2 l1)
	(error "Division by a polynomial of higher degree!")
	(let ((q (make-array (+ 2 l1 (- l2)) :initial-element 0))
	      (leading-s (leading-coef s))
	      (sneg (poly- s)))
	  (setf (main-var q) (main-var p))
	  (do* ((i l1 (length p)))
	       ((< i l2) (error "Quotient not exact"))
	    (setf (svref q (+ 1 i (- l2))) (poly/poly (leading-coef p) leading-s))
            ;(format t "step 1~%")
	    (setq p  (poly+poly p (poly*poly (subseq q 0 (+ 2 i (- l2)))
                                             sneg)))
            ;(format t "step 2~%")
	    (when (eql 0 p)
              (return (normalize-poly q))))))))

;;; Example
#+ (or)
(prefix->canon '(/ (- (/ (* K (- K 1) (- K 2) (- K 3) (- K 4)) 120)
                      (+ (* (- (/ (* K (- K 1) (- K 2)) 720)) (/ (* (+ K -4) (+ K -3)) 2))
                         (* 0 (/ (* (+ K -4) (+ K -3) (+ K -2)) 6))
                         (* (/ K 12) (/ (* (+ K -4) (+ K -3) (+ K -2) (+ K -1)) 24))
                         (* (/ 1 2) (/ (* (+ K -4) (+ K -3) (+ K -2) (+ K -1) (+ K 0))
                                       120))
                         (* (/ 1 (+ K 1))
                            (/ (* (+ K -4) (+ K -3) (+ K -2) (+ K -1) (+ K 0) (+ K 1))
                               720))))
                   (+ K -4)))
;; =>  0

#+(or)
(canon->prefix (prefix->canon '(/ (* O (+ S (* Y -1))) (+ S (* Y -1)))))

#+ (or)
(polynomial-content 
 (prefix->canon '(* 3 (+ (* K (^ N 4))
                       (* Y (^ N 3))
                       (* O (^ N 2))
                       (* S N)
                       O))))

#+(or)
(poly-remainder 
 (prefix->canon '(+ (^ O 2) (* Y S)))
 (prefix->canon 'O))

#+(or)
(prefix->canon '(+ (+ (* K N N N N)
                      (* Y N N N)
                      (* O N N)
                      (* D N)
                         O)
                   (+ (* K N N N N)
                      (* Y N N N)
                      (* O N N)
                      (* D N)
                         O)
                   (+ (* K N N N N)
                      (* Y N N N)
                      (* O N N)
                      (* D N)
                         O)))

#+(or)
(prefix->canon '(* (/ 1 (+ K 1))
                   (/ (* (+ K -4) (+ K -3) (+ K -2) (+ K -1) (+ K 0) (+ K 1))
                      720)))
;; =>  (K - 4) (K - 3) (K - 2) (K - 1) K     K^5 - 10K^4 + 35K^3 - 50K^2 + 24K
;;     ---------------------------------  =  ---------------------------------
;;                    720                                   720

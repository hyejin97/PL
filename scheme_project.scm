;; (fromTo k n) returns the list of integers from k to n. The size of the problem can be seen as the number of integers between k and n, inclusive.
;; Base Case: if k > n (i.e. if the size of the problem is 0), then the result is the empty list. 
;; Hypothesis: Assume (fromTo (+ k 1) n) returns the list of integers from k+1 to n, since the size of that problem is one less than the size of the orignal problem, (fromTo k n).
;; Recursive step: (fromTo k n) = (cons k (FromTo (+ k 1) n)

> (define (fromTo k n)
    (cond ((> k n) '())
          (else (cons k (fromTo (+ k 1) n)))))


;; (removeMults m L) returns a list containing all the elements of L that are not multiples of m.
;; Base Case : if L is empty return L
;; Hypothesis : Assume (removeMults m (cdr L)) returns the list of elements that are not multiplies of m except the first element
;; Recursive step : (removeMults m L) = (cond (modulo (car L) m) (cons (car L) (removeMults m (cdr L)))

> (define (removeMults m L)
    (cond ((null? L) L)
        (else (cond ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
              (else (cons (car L) (removeMults m (cdr L))))))))


;;(removeAllMults L), given a list L containing integers in strictly increasing order,
;;returns a list containing those elements of L that are not multiples of each other.
;;basecase : if L is empty return L
;;Assumption : Assume (removeAllMults (cdr L)) return the lists containing those elements of cdr L that are not multiples of each other.
;;Recursive step : (removeAllMults L) = (cons (car L) (removeMults((car L) (removeAllMults (cdr L)))))

> (define (removeAllMults L)
  (cond ((null? L) L)
        (else (cons (car L) (removeMults (car L) (removeAllMults (cdr L)))))))


;;(primes n) computes the list of all primes less than or equal to n.
;;primes are not multiples of any other numbers except 1 so we can use removeAllMults.
> (define (primes n)
    (removeAllMults (fromTo 2 n)))


;;(maxdepth L) returns the maximum nesting depth of any element within L, such that the topmost elements are at depth 0.
;;Basecase : L is not a list return -1. (single value has depth -1 as the shallowest list (1, 2, 3) has depth 0, not 1)
;;Hypothesis : Assume (maxdepth (car L)) returns the maximum nesting depth of the first nested element of L and (maxdepth (cdr L)) returns the maximum nesting depth of remaining elements.
;;Recursive step : (maxdepth L) = max ((1 + (maxdepth (car L))) (maxdepth (cdr L)))

> (define (maxDepth L)
    (cond ((not (list? L)) -1)
          ((null? L) 0)
          (else (let ((result1 (+ 1 (maxdepth (car L)))) (result2 (maxdepth (cdr L))))
                  (cond ((= 0 result1) (maxdepth (cdr L)))
                        (else (cond ((> result1 result2) result1)
                                    (else result2))))))))


;;(prefix exp) transforms an infix arithmetic expression exp into prefix notation.
;;Basecase : if exp is an atom then return (list exp)
;;Hypothesis : Assume (prefix (car exp)) returns the prefix notation of first atom or the expression in the first parentheses.
;;Recursive step : (prefix exp) = (list (cadr exp) (prefix (car exp)) (prefix (cddr exp))))

> (define (prefix exp)
    (cond ((not (list? exp)) exp)
          ((null? (cdr exp)) (prefix (car exp)))
          (else (list (cadr exp) (prefix (car exp)) (prefix (cddr exp))))))


;;(composition fns) returns the composition of the functions in fns.
;;Basecase : fns has only one function -> return fns
;;Assumption : (composition (cdr fns)) returns a function which is composition of the functions except the first function in fns
;;Recursive step : (composition fns) = (lambda (x) ((car fns) ((composition (cdr fns)) x)))

> (define (composition fns)
    (cond ((not (list? fns)) fns)
        ((null? (cdr fns)) (composition (car fns)))
        (else (lambda (x) ((car fns) ((composition (cdr fns)) x))))))


;;(bubble-to-nth L N) returns a list containing all the elements of L after bubbling the largest number among the first N elements to the N-th point
;;Basecase : if N = 1 no need to bubble up. Return the list L.
;;Assumption : (bubble-to-nth (cdr L) (- N 1)) bubbles the largest number among the first N items except the first item.
;;Recursive step : (bubble-to-nth L N) = (if (< (car L) (cadr L)) (cons (car L) (bubble-to-nth (cdr L) (- N 1)))
;;                                       (else (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1)))

> (define (bubble-to-nth L N)
  (cond ((= N 1) L)
        (else (if (< (car L) (cadr L))
                  (cons (car L) (bubble-to-nth (cdr L) (- N 1)))
                  (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1)))))))


;;(b-s L N) returns the a list containing the elements of L in their original order except that the first N elements are in sorted order.
;;Basecase :if N = 1, return L
;;Assumption : (b-s L N-1) returns a list with the first N-1 elements are in sorted order.
;;Recursive step : (b-s L N) = (b-s (bubble-to-nth L N) N-1)

> (define (b-s L N)
  (cond ((= N 1) L)
        (else (b-s (bubble-to-nth L N) (- N 1)))))


;;(bubble-sort L) return a list of the elements of L in sorted order.

> (define (bubble-sort L)
    (b-s L (length L)))


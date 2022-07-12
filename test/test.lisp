(defpackage :hbook.test
  (:use :cl :1am :hbook :cl-oju)
  (:export :run-tests))

(in-package #:hbook.test)

(defun run-tests () (1am:run))

(test minmax-test
  (is (equal '(1 1)
             (minmax '(1))))
  (is (equal '(1 2)
             (minmax '(2 2 1 1))))
  ;; This is strange to me, but apparently ECL and SBCL both set
  ;; min/max to zero in LOOP if there is no data:
  (is (equal '(0 0)
             (minmax nil))))

(test rtrim-test
  (is (equal "" (rtrim "")))
  (is (equal "" (rtrim "   ")))
  (is (equal "hi" (rtrim "hi   ")))
  (is (equal "  hi" (rtrim "  hi   "))))

(test bin-index-test
  (is (= 0 (bin-index 2 0 1 0)))
  (is (= 1 (bin-index 2 0 1 1)))
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10)
             (loop for el in '(2 3 4 5 6 7 8 9 10 11 12)
                   collect (bin-index 11 2 12 el)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apply-bindings (bindings case expr)
    "
    (apply-bindings '(x y) '(2 3) '(+ x y))

    ;;=>
    '(+ 2 3)
    "
    (loop with ret = expr
          for b in bindings
          for c in case
          do (setf ret (subst c b ret))
          finally (return ret))))

(defmacro are (argv expr &rest cases)
  "
  Analog of clojure.test/are.  Apply multiple assertions
  based on some set up code and variations of one or more
  variable bindings.
  "
  (let ((c (length argv)))
    `(progn ,@(loop for case in (partition-n c c cases)
                    collect (apply-bindings argv case expr)))))

(test hist-values-test
  (are (h c mn mx heights xs)
       (progn
         (is (= c (hist-count h)))
         (is (= mn (hist-min h)))
         (is (= mx (hist-max h)))
         (is (equalp heights (hist-bin-heights h)))
         (is (equalp xs (hist-bin-xs h))))
       (histogram '(1 2) 2)     2 1 2 #(1 1)   #(1 2)
       (histogram '(1 2 1) 2)   2 1 2 #(2 1)   #(1 2)
       (histogram '(1 2 2 3) 3) 3 1 3 #(1 2 1) #(1 2 3)))

(test vertical-num-labels-test
  (are (expected space xs decimalp)
       (is (equal (format nil expected)
                  (vertical-num-labels space xs decimalp)))
       "1"     0 #(1)    nil
       "  1"   2 #(1)    nil
       "11"    0 #(1 1)  nil
       "12"    0 #(1 2)  nil
       "1~%0"  0 #(10)   nil
       "1~%01" 0 #(10 1) nil
       "123~%123~%123" 0 #(111 222 333) nil
       ;; Decimal point added:
       "1~%.~%2~%3"                   0 #(1.23)                 t
       "123~%123~%123~%...~%123~%123" 0 #(111.11 222.22 333.33) t))

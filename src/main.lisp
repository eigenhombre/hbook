(in-package #:hbook)

(defun d () (1+ (random 6)))

(defun dn (n)
  (loop repeat n
        sum (d)))

(dn 2)

(dn (d))

(dn (dn (dn (dn (d)))))

(defun dr (n)
  (loop with acc = (d)
        repeat (1- n)
        do (setf acc (dn acc))
        finally (return acc)))

(dr 1)
(dr 2)
(dr 5)
(dr 10)
(loop repeat 100 collect (dr 5))

;; Utilities
(defun minmax (list)
  (loop for el in list
        maximizing el into max
        minimizing el into min
        finally (return (list min max))))

(defun rtrim (s) (string-right-trim '(#\Space) s))

;; Histogram Data Functions
(defun bin-index (nbins min max el)
  "
  For a given element el between min and max in a histogram with nbins bins,
  find the index of the bin for that element.
  "
  (round (* (1- nbins)
            (/ (- el min)
               (- max min)))))

(defun bin-value (nbins min max i)
  "
  For the i'th bin in a histogram of nbins bins whose min and max values are given,
  find the corresponding x value of the center of the bin.
  "
  (float (+ min (/ (* i (- max min))
                   (1- nbins)))))

;; Simple API for now.  Use struct or CLOS?
(defun make-histo (min max bin-width bin-heights bin-values)
  (list min max bin-width bin-heights bin-values))

(defun hist-values (numlist nbins)
  (assert numlist)
  (destructuring-bind (min max) (minmax numlist)
    (let ((bin-width (/ (1+ (- max min)) nbins))
          (bin-heights (make-array nbins :element-type 'integer))
          (bin-xs (make-array nbins :element-type 'integer)))
      (loop for i below nbins
            do (setf (elt bin-xs i) (bin-value nbins min max i)))
      (loop for el in numlist
            do (incf (elt bin-heights (bin-index nbins min max el))))
      (make-histo min
                  max
                  bin-width
                  bin-heights
                  bin-xs))))

(defun hist-bin-heights (histo)
  (nth 3 histo))

(defun hist-bin-xs (histo)
  (nth 4 histo))

(hist-values '(0 1 2) 3)
(hist-values '(-1 0 1) 3)
(hist-values '(-2 0 2) 3)
(hist-values '(1 2 2 3) 3)
(hist-values '(10 20 20 30) 3)
(hist-values (loop repeat 10000 collect (dr 6)) 30)
(hist-values (loop repeat 100000 collect (dn 100)) 10)

(defun hist-str (histo &optional (binheight 10))
  (let* ((bins (coerce (hist-bin-heights histo) 'list))
         (maxbin (apply #'max bins)))
    (format nil "~{~A~%~}"
            (loop for h from (1- binheight) downto 0
                  collect
                  (rtrim
                   (format nil "~10d ~{~A~}"
                           (round (/ (* h maxbin)
                                     binheight))
                           (loop for binval across (hist-bin-heights histo)
                                 collect
                                 (if (< (/ h binheight)
                                        (/ binval maxbin))
                                     #\X
                                     #\Space))))))))

(defun nspaces (n)
  (format nil "~v@{~A~:*~}" n " "))

(defun pad-num (width num)
  (format nil "~A~A" (nspaces width) num))

(defun count-strs (counts decimalp)
  (let* ((as-chars (loop for c across counts
                         collect (format nil (if decimalp
                                                 "~,2F"
                                                 "~a")
                                         c)))
         (max-len (apply #'max (mapcar #'length as-chars))))
    (list max-len
          (mapcar (lambda (s)
                    (if (equal s "0")
                        (nspaces max-len)
                        (pad-num (- max-len (length s)) s)))
                  as-chars))))

(defun vertical-num-labels (lspace vals &optional (decimalp nil))
  (destructuring-bind (max-len padded-strs) (count-strs vals decimalp)
    (format nil "~{~A~^~%~}"
            (loop for i from 0 below max-len
                  collect (rtrim
                           (format nil "~A~{~A~}"
                                   (nspaces lspace)
                                   (loop for j from 0 below (length vals)
                                         collect (elt (elt padded-strs j) i))))))))

(defun hbook (vals &optional (nbins 50) (height 5))
  (let* ((hist (hist-values vals nbins))
         (hstr (hist-str hist height))
         (vclabels (vertical-num-labels 11 (hist-bin-heights hist)))
         (vxlabels (vertical-num-labels 11 (hist-bin-xs hist) t)))
    (format nil "~A~%~A~%~%~A" hstr vclabels vxlabels)))

(defun pr (&rest s)
  (format t "~{~A~}~%" s))

(pr (hbook '(1 2 2 3) 3 5))
(pr)
(pr (hbook '(-1 0 1) 3 5))
(pr)
(pr (hbook (loop repeat 100000 collect (dn 2))
           11
           20))
(pr)
(pr (hbook (loop repeat 100000 collect (dn 100))
           100
           10))
(pr)
(pr (hbook (loop repeat 30000 collect (dn 3000))
           100
           30))
(pr)
(pr (hbook (loop repeat 1000 collect (dr 10))))
(pr)
(pr (hbook (loop repeat 1000000
                 collect (* 50 (- (log (random 1.0)))))
           50
           20))

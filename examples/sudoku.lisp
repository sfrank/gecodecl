;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; utility functions for Sudoku CSP modelling

(defun chunk (size list)
  (declare (type (integer 0 *) size))
  (let (chunks)
    (loop for l = list then (subseq l size)
          while l do (push (subseq l 0 size) chunks)
          finally (return (nreverse chunks)))))

(defun matrix-transpose (matrix)
  (when matrix
    (apply #'mapcar #'list matrix)))

(defun rows (list &optional (size 9))
  (chunk size list))

(defun columns (list &optional (size 9))
  (matrix-transpose (chunk size list)))

(defun boxes (list &optional (size 3))
  (flet ((flatten (list)
           (apply #'append list)))
    (flatten
     (mapcar (lambda (x)
               (mapcar #'flatten
                       (matrix-transpose x)))
             (chunk size 
                    (chunk size 
                           (chunk size list)))))))

(defun print-sudoku (list &optional (stream t))
  (let ((control-string 
         "~{~A~^ ~#[~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ------+-------+------~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ------+-------+------~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~;~;~;| ~;~;~;| ~;~;~;~%~
                    ~:;~]~}~3%" ))
    (format stream control-string list)))


;;; Sudoku problems

(defparameter *sudoku-problem*
  '(_ _ 8   _ _ _   6 _ _ 
    _ 4 _   9 _ 2   _ 5 _ 
    _ _ _   6 4 8   _ _ _ 

    _ 3 9   _ 2 _   1 7 _ 
    _ 1 _   _ _ _   _ 3 _ 
    _ 8 5   _ 1 _   2 6 _ 
    
    _ _ _   2 8 7   _ _ _ 
    _ 6 _   1 _ 4   _ 8 _ 
    _ _ 2   _ _ _   5 _ _))


;; the accompanying sudokus.txt is from Peter Norvig's Sudoku
;; experiments at http://norvig.com/sudoku.html
(defun read-sudokus (&optional (file "examples/sudokus.txt"))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          collect (map 'list #'digit-char-p line))))

;;; modelling

(defun sudoku (grid)
  (with-gecode
    (let ((v (integer-seq grid :min 1 :max 9)))
      (dolist (list (append (rows v) (columns v) (boxes v)))
        (distinct-g list))
      (one-solution v #'make-dfs))))

(defun sudoku-stress ()
  (loop for s in (list* *sudoku-problem* (read-sudokus))
        collect (sudoku s)))

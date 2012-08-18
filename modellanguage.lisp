;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GECODECL; Base: 10 -*-

(in-package :gecodecl)

;;; Boolean and Integer Relations

;; TODO

(defun <g (&rest args &key (clevel :icl-def) (space *gspace*))
  (let* ((v (loop for i in args
                 until (keywordp i)
                 collect i))
         (length (length v)))
    (cond 
      ((= length 1)
       (rel-post :irt-< (first v) nil nil clevel space))
      ((= length 2)
       (rel-post :irt-< (first v) (second v) nil clevel space))
      ((= length 3)
       (rel-post :irt-< (first v) (second v) (third v) clevel space))
      (t
       (error "....")))))

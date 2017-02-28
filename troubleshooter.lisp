;;;; troubleshooter.lisp

(in-package #:troubleshooter)

(defvar *decision-tree* nil)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defparameter *trouble-tree*
  '(0
    (1 (4 11) (5 12))
    (2 (6 13) (7 14))
    (3 (8 15) (9 16) (10 16))))

(defparameter *node-contents*
  #("What's the problem?"
    "Head hurts"
    "Leg hurts"
    "Arm hurts"
    "Really bad"
    "Not so bad"
    "I am Dr. House"
    "I am Not Dr. House"
    "Partially numb"
    "In pain"
    "Totally numb"
    "Take opiates"
    "Take Tylenol"
    "Take opiates, make others miserable"
    "Amputate"
    "You're fine, you just slept on it funny"
    "You're having a heart attack"))

(defun run-tree (tree)
  (cond ((= 2 (length tree))
         (format t "~a~%"(node-content (second tree))))
        (t
         (format t "~a~%" (node-content (first tree)))
            (show-choices tree)
            (run-tree
             (elt (rest tree)
                  (1- (parse-integer (prompt-read ""))))))))

(defun node-content (node)
  (elt *node-contents* node))

(defun show-choices (tree)
  (cond ((numberp (first (rest tree))) nil)
        (t (loop for item in (rest tree)
              for i from 1 to (length (rest tree))
              do (format t
                         "~d. ~a~%"
                         i
                         (node-content (first item)))))))

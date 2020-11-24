(defvar *l* nil) 
 
(defun nqueen (m) 
  (setq *l* m) 
  (queen m nil)) 
 
(defun queen (n b) 
  (cond ((zerop n) nil) 
        ((or (member n b) (qp 1 b n)) 
         (queen (1- n) b)) 
        (t (nconc (cond ((eql (length b) (1- *l*)) 
                         (list (cons n b))) 
                        (t (queen *l* (cons n b)))) 
                  (queen (1- n) b))))) 
 
(defun qp (k m n) 
  (cond ((null m) nil) 
        ((eql k (abs (- n (car m)))) t) 
        (t (qp (1+ k) (cdr m) n))))

;;; flip on Y-axis
(defun flip (a)
  (reverse a))

;;; rotate 90 degree to left
(defun rotate (a)
  (rot a (length a) (length a)))

(defun rot (a l m)
  (cond ((zerop l) nil)
        (t (cons (- m (length (cdr (member l a))))
               (rot a (1- l) m)))))

(defun display (a)
  (cond ((null a))
        (t 
         (do ((i 1 (incf i)))
             ((= i (car a)))
           (format T "  "))
         (format T "Q~%")
         (display (cdr a)))))

(defun synonymp (a b)
  (block nil
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (setq b (flip b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (setq b (rotate b))
    (if (equal a b) (return t))
    (return nil)))
 
(defun uniq (a)
  (let ((result nil))
    (mapcar #'(lambda (x)
                (if (notany #'(lambda (s) (synonymp s x)) result)
                    (push x result)))
            a)
    result))


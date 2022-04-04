;发现错误：同级运算先后顺序错误

(defun norm-level (a);;;
  (cond ((equal a 'expt) 0)
	((or (equal a '*) (equal a '/)) 1)
	((equal a 'mod) 2)
	((or (equal a '+) (equal a '-)) 3)
	((or (equal a '=) (equal a '/=)) 4)
	((equal a 'not) 5)
	((equal a 'and) 6)
	((equal a 'or) 7)
	(t 8)))

(defmacro tempf (a b);;;
  `(progn
     (let ((te ,a))
       (setf ,a ,b)
       (setf ,b te))))

(defun back-to-forth (list);;;
  (let ((ans nil))
    (dolist (i list ans)
      (push i ans))))

(defun sublist (list i j)
  (let (ans)
    (back-to-forth (dotimes (k (1+ (- j i)) ans)
		     (push (nth (+ k i) list) ans)))))		   

(defun ne-sort (list)
  (let ((amts (make-array 8 :initial-element 0))
	(bracket-amt 0)
					;the amount of bracket couples
	(bracket-i 0)
					;the first place of [
	(ans nil)
					;temp list
	(cnt 0))
    (dotimes (i (length list))
      (if (and (= bracket-amt 0) (< (norm-level (nth i list)) 8))
	  (progn (incf (elt amts (norm-level (nth i list))))
		 (setq ans (append ans `(,(nth i list)))))
	  (if (equal (nth i list) '])
	      (if (= bracket-amt 0)
		  (progn (incf bracket-amt)
			 (setf bracket-i i))
		  (incf bracket-amt))
	      (if (equal (nth i list) '[)
		  (progn (decf bracket-amt)
			 (if (= bracket-amt 0)
			     (setq ans (append (sublist ans 0 (1- bracket-i)) '(]) (ne-sort (sublist list (1+ bracket-i) (1- i))) '([)))))
		  (setq ans (append ans `(,(nth i list))))))))
    ;(print amts);
    ;(print ans);
    (dotimes (n-l 8 ans);(print ans));
      (dotimes (i (elt amts n-l))
	(progn (setq bracket-amt 0 cnt 0)
	       (dotimes (j (1- (length list)))
		 (if (equal (nth j ans) ']) (incf bracket-amt))
		 (if (equal (nth j ans) '[) (decf bracket-amt))
		 (if (and (= n-l (norm-level (nth j ans))) (= 0 bracket-amt)) (incf cnt))
		 (if (< i cnt)
		     (if (or (> (norm-level (nth j ans)) (norm-level (nth (1+ j) ans)))
			     (= (norm-level (nth (1+ j) ans)) 8)
			     (> bracket-amt 0))
			 (progn (tempf (elt ans j) (elt ans (1+ j)))
				(if (equal (nth j ans) ']) (incf bracket-amt))
				(if (equal (nth j ans) '[) (decf bracket-amt)))
			 (return)))))))))
		     

;;objective function without ()s
(defun ne-list (list)
  (remove-if (lambda (x) (or (equal x '[) (equal x ']))) (back-to-forth (ne-sort (back-to-forth list)))))


(defun ne-inner (list)
  (let* (item
	 (newlist list)
	 ans1
	 ans2)
    ;(print "1:")
    ;(print newlist)
    (setq item (pop newlist))
    ;(print "2:")
    ;(print newlist)
    (if (= (norm-level item) 8)
	`((,item) ,newlist)
	(progn
	  (setq ans1 (ne-inner newlist) newlist (second ans1) ans2 (ne-inner newlist) newlist (second ans2))
	  (cond ((= 1 (length (first ans1)) (length (first ans2))) 
		 `(,(append `(,item) (first ans1) (first ans2)) ,newlist))
		((= 1 (length (first ans1)))
		 `(,(append `(,item) (first ans1) `(,(first ans2))) ,newlist))
		((= 1 (length (first ans2)))
		 `(,(append `(,item) `(,(first ans1)) (first ans2)) ,newlist))
		(t `(,(append `(,item) `(,(first ans1)) `(,(first ans2))) ,newlist)))))))
		 
;;objective function
(defun ne-fun (list)
  (first (ne-inner (ne-list list))))

(defvar ne-reserved-variable nil)
(defmacro ne-macro ()
  ne-reserved-variable)

;objective final macro, can use symbols, numbers and operators!!
(defmacro ne (list)
  `(progn
     (setq ne-reserved-variable (ne-fun ,list))
     (ne-macro)))

;;but why the following is wrong???
;;(defun (list)
;;(setq ne-reserved-variable (ne-fun list))
;;(ne-macro))



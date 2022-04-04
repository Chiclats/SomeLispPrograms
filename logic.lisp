(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))
					;用于判断是否是变量

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b (or (binding (cdr b) binds)
	      (cdr b)))))
					;用来找到binds库中x绑定的

(defun match (x y &optional binds)
  (cond ((eql x y) (values binds t))
	((assoc x binds) (match (binding x binds) y binds))
	((assoc y binds) (match x (binding y binds) binds))
	((var? x) (values (cons (cons x y) binds) t))
	((var? y) (values (cons (cons y x) binds) t))
	(t (when (and (consp x) (consp y))
	     (multiple-value-bind (b2 yes)
		 (match (car x) (car y) binds)
               (and yes (match (cdr x) (cdr y) b2)))))))
					;用来比较两个列表并且将对应位置的元素相互绑定

(defvar *rules* (make-hash-table))
					;用来存放事实与推导规则

(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))
					;用来导入事实与推导规则，con为结论，ant为前提，事实只有结论没有前提

(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or  (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t   (prove-simple (car expr) (cdr expr) binds))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes)
                  (match args (car r) binds)
                (when yes
                  (if (cdr r)
                      (prove (cdr r) b2)
                      (list b2)))))
          (mapcar #'change-vars
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")));产生未被定义的以?开头的符号
                  (vars-in r))
          r))
					;用gensym产生出来的符号替换r中所有变量

(defun vars-in (expr)
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
             (vars-in (cdr expr)))))
					;用来找到expr中所有的var
(defun prove-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (prove (car clauses) b))
              (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query))
         ,@body))))

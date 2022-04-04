(load "d:/emacs-27.2-x86_64/programs/gmath/gmath-linearAl.lisp")
					;这是引用一个库
(setq *READ-DEFAULT-FLOAT-FORMAT* 'double-float)
					;这是把数据改成双精度类型

(defvar state-list (make-list 2500 :initial-element '(0 0)))
					;这是用来存放10000个粒子的位置的数组

(defun reset-list ()
  (dotimes (i 50)
    (dotimes (j 50)
      (setf (nth (+ j (* 50 i)) state-list) `(,(+ 0.5 i) ,(+ 0.5 j))))))
					;重置粒子的位置,每个粒子最初都在（0.5+i，0.5+j）格点上

(defvar cooling-r);降温率
(defvar ini-temp 300);初始温度
(defvar fin-temp 30);终止温度
(defvar r 0.1);粒子的半径（圆形）
(defvar mag '(0 1));每个粒子的磁矩
;;假定在外场下的磁化磁矩为一个固定值mag向量(0,1)

(defun f-b-t-v (x &optional (k 'start))
	       ;用于找到分界点，以确定x∈某个范围的state-list区间，'start(默认)用来找比x大的第一个位置，'end用来找比x小的最后一个位置
  (let ((L 0)
	(R 2499)
	M)
    (do ()
	((< R L) (if (equal k 'start) L R));是个二分法，需要保证x排序过
      (setq M (floor (/ (+ L R) 2)))
      (if (equal k 'start)
	  (if (>= (first (nth M state-list)) x)
	      (setq R (1- M))
	      (setq L (1+ M)))
	  (if (<= (first (nth M state-list)) x)
	      (setq L (1+ M))
	      (setq R (1- M)))))))

(defun psbp (x y n)
					;用于判断一个位置是否合法
  (and (<= (+ x r) 50)
       (>= (- x r) 0)
       (<= (+ y r) 50)
       (>= (- y r) 0)
       (loop for i from (f-b-t-v (- x r)) to (f-b-t-v (+ x r) 'end)
	     always (or (= n i)
			(<= (* 2 r) (sqrt (inner_* (vector_- `(,x ,y) (nth i state-list))
						   (vector_- `(,x ,y) (nth i state-list)))))))))

(defun step-leng (temp)
					;给定温度计算每一步的长度，可修改成更加符合实际的模块，目前只是步长正比于根号temp
  (/ (sqrt temp) cooling-r))

(defun energy (x y n)
					;计算选定的磁矩与周边临近磁矩间的相互作用能，为减少算量只计算距离小于10的粒子的相互作用能
  (let ((st (f-b-t-v (- x 10)))         ;st为横坐标比x-10大的第一个粒子序号
	(ed (f-b-t-v (+ x 10) 'end)))   ;ed为。。。比x-10小的最后一个。。。
    (loop for i from st to ed           ;一个循环求和能量
	  sum (if (= n i) 0 (- (/ (inner_* mag mag) (expt (inner_* (vector_- `(,x ,y) (nth i state-list))
								   (vector_- `(,x ,y) (nth i state-list)))
							  3/2))
			       (* (/ (expt (inner_* mag (vector_- `(,x ,y) (nth i state-list))) 2)
				     (expt (inner_* (vector_- `(,x ,y) (nth i state-list))
						    (vector_- `(,x ,y) (nth i state-list)))
					   5/2))
				  3))))))

(defun refresh-state-list (n dir)
					;用于排序state-list，按照x坐标从小到大，n是位置发生变动的粒子的序号，dir代表方向，正变大负变小
  (let (exc-var
	(di (if (> dir 0) 1 -1)))       ;赋值di=sgn(dir)
    (do ()                              ;循环，直到移动到正确的位置
	((or (and (= di 1)
		  (= n 2499))
	     (and (= di -1)
		  (= n 0))
	     (< (* di (- (first (nth n state-list)) (first (nth (+ n di) state-list)))) 0)))
      (setf exc-var (nth n state-list)
	    (nth n state-list) (nth (+ n di) state-list)
	    (nth (+ n di) state-list) exc-var
	    n (+ n di)))))

(defun make-fig (&optional (steps 1000))
					;计算图像变化的函数
  (dotimes (i steps);循环steps步，计数变量是i
    (let* ((n 0);内层循环计数变量
	   temp ran x y dist ang xn yn ac-r rec-l)
	   ;声明变量：分别为温度 随机选定的粒子序号 初始位置（x,y） 运动位移、角度 运动后的位置 是否接受的随机变量 记录这一轮循环已经运动过的粒子位置的数组
      (tagbody
	 (setq temp (+ fin-temp (* (- ini-temp fin-temp) (- steps i) (/ steps))));计算这一步的温度
       res;这是内层循环起始的标志
	 (setq ran (random 2500));随机选定一个移动的粒子的序号ran
	 (setq x (first (nth ran state-list)))
	 (setq y (second (nth ran state-list)))
	 (setq dist (step-leng temp));根据温度计算位移
	 (setq ang (random (* 2 pi)));随机出一个运动方向
	 (setq xn (+ x (* dist (cos ang))) yn (+ y (* dist (sin ang))));计算运动后位置
	 (if (and (psbp xn yn ran);判断是否为合法位置
		  (loop for li in rec-l always (< 10 (sqrt (inner_* (vector_- `(,x ,y) li) (vector_- `(,x ,y) li))))))
					;选定的粒子跟已经移动过的粒子的间距应该大于10
	     (let ((Ei (energy x y ran));计算运动前能量
		   (Ef (energy xn yn ran)));计算运动后能量
	       (if (< Ef Ei);如果运动后能量更低
		   (progn;保存这个变化
		     (setq rec-l (push `(,xn ,yn) rec-l));写到rec-l里面
		     (setf (nth ran state-list) `(,xn ,yn));写到state-list里面
		     (incf n));n++
		   (progn;否则
		     (setq ac-r (random 1.0000));随机一个[0,1]的数ac-r
		     (if (< ac-r (exp (/ (- Ei Ef) (* 0.01 temp))))   ;设kB=0.01
			 (progn;如果ac-r小于exp((Ei-Ef)/(kB*temp)),保存这个变化
			   (setq rec-l (push `(,xn ,yn) rec-l))
			   (setf (nth ran state-list) `(,xn ,yn))
			   (incf n))
			 (go res)))));否则放弃这个变化
	     (go res));位置不合法，放弃这个变化
	 (if (\= (cos ang) 0) (refresh-state-list ran (cos ang)));改变位置
	 (if (< n 5) (go res))));一个goto循环，重复五次，开始的标记为res
      ;(setq state-list (sort state-list '< :key #'first)));按照x的大小排列state-list
    (if (= (mod i 100) 0) (print i))))

(defvar f);文件输出流

(defun link-with-file (file-name);写入文件,把数据转换成一个matlab代码
  (setq f (open file-name :direction :output :if-exists :rename :if-does-not-exist :create))
  (format f "n1=[")
  (dotimes (i 2500) (format f "~a " (first (nth i state-list))))
  (format f "];~%")
  (format f "n2=[")
  (dotimes (i 2500) (format f "~a " (second (nth i state-list))))
  (format f "];~%")
  (format f "scatter(n1,n2,5)")
  (close f))
  
(defun one-exp (steps file-name);main
  (reset-list)
  (make-fig steps)
  (link-with-file file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defvar state-list
;  (let (temp)
;    (setf temp (make-list 10000 :initial-element 0))
;    (setf temp (push '* temp))
;    (concatenate 'list (make-list 10000 :initial-element 0) temp)))
;(defvar state-mtx (smtx state-list))
;
;(defun reset-mtx ()
;  (dotimes (i 100)
;    (dotimes (j 100)
;      (setel state-mtx 0 (+ (* 100 i) j) (+ i 0.5))
;      (setel state-mtx 1 (+ (* 100 i) j) (+ j 0.5)))))

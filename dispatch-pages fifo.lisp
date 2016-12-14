
;; pages dispatch
(defparameter *instruction-list* ())
(defparameter *temp* ())
(let ((cout 0))                             ;构造32个指令页面
  (loop for i from 0 upto 319
     do(progn
	 (push i *temp*)
	 (incf cout)
	 (when (= ( mod cout  10) 0)
	   (setf *temp* (reverse *temp*))
	   (push *temp* *instruction-list*)
	   (setf *temp* ()))))
  	   (setf *instruction-list* (reverse *instruction-list*)))
(defparameter *process-list* ())
(defun get-instruction (x y)        ;产生随机指令
	   (let ((num 0 ))
	     (setf num (random y))
	     (if (< num x)
		 (get-instruction x y)
		 num)))
(defun construct-process-list()      ;构造一个要运行的指令序列
  (loop for i from 0 upto 79
     do(progn
	 (push i *process-list*)
	  (push (+ i (random 10)) *process-list*)
	 (push (get-instruction 0 160) *process-list*)
	 (push (get-instruction 160 320) *process-list*))))
(defparameter *own-memory* (make-array 4 :initial-element nil))



(defun get-instruction-posistion (instru-num) ;寻找指令所在的页面数
  (let ((flag 0))
  (dotimes (i  (length *instruction-list*))
    (dolist (lst1 (nth i *instruction-list*))
      (when (= instru-num lst1)
	(setf flag 1)
	(return )))
    (when (= flag 1)
      (return i)))))


(defun judge-posistion(run-num)        ;判断该执行的指令是否在内存中
  (let ((flag 0))
    (dotimes (i (length *own-memory*))
      (dolist (lst (aref *own-memory* i)) 
	(when (= run-num lst)
	  (setf flag 1)
	  (return)))
      (if (= flag 1)
	  (return t)))))
(defun show-page(run-num)                  ;执行的指令，以及当前执行到第几条指令
  (let ((flag 0))
  	(format t "执行的指令 (存在于内存的)：~a ~% ~%" run-num)
		  (dotimes (k (length *own-memory*))
		    (dolist (lst (aref *own-memory* k)) 
		      (when (= run-num lst)
			(setf flag 1)
			(return)))
		    (if (= flag 1)
			(progn
			  (format t "对应的页面~a ~%~%" (aref *own-memory* k))
			  (return))))))
			  

(defun fifo()                                   ;先进先出算法
  (construct-process-list)
  (let ((tag 0)
	(count 0))
    (dotimes ( i 320)
      (if ( < i 4)	
	  (if (judge-posistion (nth i *process-list*))
	    (show-page (nth i *process-list*))
	     (progn
	        (format t "执行的指令(不存在于内存的) ：~a~%~%" (nth i *process-list*))
		        (format t "执行之前的内存情况 ：~a ~%~%" *own-memory*)
		(setf (aref *own-memory* tag) (nth (get-instruction-posistion (nth i *process-list*)) *instruction-list*))
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)
		(incf tag)
		(when (= tag 4)
		  (setf tag 0))
		(incf count)))
	  (if (judge-posistion (nth i *process-list*))
	      (show-page (nth i *process-list*))
	      (progn
		(format t "执行的指令(不存在于内存的) ：~a~%~%" (nth i *process-list*))
		(format t "执行之前的内存情况 ：~a ~%~%" *own-memory*)
		(setf (aref *own-memory*  tag) (nth (get-instruction-posistion (nth i *process-list*)) *instruction-list*))
		(incf count)
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)
		(incf tag)
		(when (= tag 4)
		  (setf tag 0))))))
      (format t "缺页率 ~a ~%   " (/ count  320)))) 

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-instruction (storag-runout-list storag-pre-list process-list k)       ;遍历寻找对应已经运行的指令
  (dolist (lst1 storag-runout-list )
    (dolist (lst2 (nthcdr k process-list))
      (when (= (car lst1 ) lst2)
	(push lst2 storag-pre-list)               ;保存到一个列表里
	(return))))
  (format t "遍历得到的指令顺序：~a~%" storag-pre-list)
  (format t "已经执行的指令序列:~a~%" storag-runout-list)
  (setf storag-pre-list (reverse storag-pre-list)))

(defun replace-page (storag-pre-list storag-runout-list k)        ;交换页面函数
  (let ((temp (random 4)))
    (when (= (length storag-pre-list) 0)          ;如果返回的查询序列为空
	  (setf (aref *own-memory* temp)
		(nth (get-instruction-posistion (nth k *process-list*)) *instruction-list*))   ;随意替换一个页面
	  (setf (nth temp storag-runout-list) (list (nth k *process-list*) temp)))
    (when (= (length storag-pre-list ) 4)
      (dolist (lst  storag-runout-list)
	(when (= (car lst ) (car (last storag-pre-list)))
	  (setf (aref *own-memory* (cadr lst)) (nth (get-instruction-posistion (nth k *process-list*)) *instruction-list*))
	  (setf lst (list (nth k *process-list*) (cadr lst ))))))
    (when (and (> (length storag-pre-list) 0)
	       (< (length storag-pre-list) 4))
      	(dolist (lst storag-runout-list)                                                ;如果返回的查询序列为不为空，将最后一个要执行的序列所对应的页面置换
	  (if (or ( = (car lst) (car storag-pre-list))   ;找到最后将要执行的指令
		  (= (car lst) (cadr storag-pre-list))
		  (= (car lst) (caddr storag-pre-list)))
	      nil
	      (progn
		(setf (aref *own-memory* (cadr lst)) (nth (get-instruction-posistion (nth k *process-list*)) *instruction-list*)) ;替换页面
		(setf lst  (list (nth k *process-list*) (cadr lst)))      ;修改保存已经执行的指令列表
		(return)))))))
 

(defun opt()
  (construct-process-list)     ;构造指令序列
  (let (( storag-runout-list ())
	(storag-pre-list ())
	(array-tag 0)
	(count 0))
    (dotimes (i 320)
      (find-instruction storag-runout-list storag-pre-list *process-list* i)
      (format t "执行第:  ~a 条指令~%" (1+ i))
      (if (< i 4)
	  (if (judge-posistion (nth i *process-list*))
	      (progn
		(show-page (nth i *process-list*))
		(setf storag-pre-list nil))
	      (progn
		(format t "执行的指令(不存在于内存的) ：~a~%" (nth i *process-list*))
		(format t "执行之前的内存情况 ：~a ~%" *own-memory*)
		(setf (aref *own-memory* array-tag) (nth (get-instruction-posistion (nth i *process-list*)) *instruction-list*))
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)
		(push (list (nth i *process-list* ) array-tag) storag-runout-list)
		(incf array-tag)
		(when (= array-tag 4)
		  (setf array-tag 0))
		(incf count)
		(setf storag-pre-list nil)))
	  (if (judge-posistion (nth i *process-list*))
	      (progn
		(show-page (nth i *process-list*))
	
		(setf storag-pre-list nil))
	      (progn
		(format t "执行的指令(不存在于内存的) ：~a~%" (nth i *process-list*))
		(format t "执行之前的内存情况 ：~a ~%" *own-memory*)
		(if (< (length storag-runout-list) 3)
		    (push (list (nth i *process-list* ) array-tag) storag-runout-list)
		(replace-page storag-pre-list storag-runout-list i))
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)
		(incf count)
		(setf storag-pre-list nil)))))
    (format t "缺页率 ： ~a " (/ count 320))))
(defparameter *a* ())
(defun longtime-not()
  (construct-process-list)
  (let ((tag 0)
	(count 0)
	(array-tag-list '(0 0 0 0 ))
	(temp ()))
    (dotimes ( i 320)
      (format t "执行第~a :条指令" i)
      (if ( < i 4)	
	  (if (judge-posistion (nth i *process-list*))
	    (show-page (nth i *process-list*))
	     (progn
	        (format t "执行的指令(不存在于内存的) ：~a~%" (nth i *process-list*))
		(format t "执行之前的内存情况 ：~a ~%" *own-memory*)
		(setf (aref *own-memory* tag) (nth (get-instruction-posistion (nth i *process-list*)) *instruction-list*))
		(setf (nth i array-tag-list) i)
		(dolist (lst array-tag-list)
		  (if (= lst i)
		      nil
		      (incf lst)))
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)
		(incf tag)
		(when (= tag 4)
		  (setf tag 0))
		(incf count)))
	  (if (judge-posistion (nth i *process-list*))
	      (show-page (nth i *process-list*))
	      (progn
		(format t "执行的指令(不存在于内存的) ：~a~%" (nth i *process-list*))
		(format t "执行之前的内存情况 ：~a ~%" *own-memory*)
		(dolist (lst array-tag-list)
		  (push lst temp))
		(setf temp (reverse temp))
		(setf tag (car (sort  temp #'>)))
		(format t" array-taglist :~a~% temp: ~a" array-tag-list temp)
		(dotimes (k 4)
		  (when (= tag (nth k array-tag-list))
		    (setf (aref *own-memory* k) (nth (get-instruction-posistion (nth i *process-list*)) *instruction-list*))
		    (setf (nth k array-tag-list) 0)
		    (return)))
		(setf temp ())
		(format t" array-taglist 1 :~a~%" array-tag-list)
		(dotimes (i 4)
		  (setf (nth i array-tag-list) (incf (nth i  array-tag-list))))
		(format t" array-taglist 2 :~a~%" array-tag-list)
		(incf count)
		
		(format t "执行之后的内存情况 ：~a ~%~%" *own-memory*)))))
	    	       
    (format t "缺页率 ~a ~%   " (/ count  320)))) 

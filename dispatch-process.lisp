;;              定义进程，初始化他们的参数
(defparameter *process-0*                
  '(:id 0  :priority 9 :cputime 0 :alltime 3 :startblock 2 :blocktime 3 :state 1))
(defparameter *process-1*
  '(:id 1  :priority 38 :cputime 0 :alltime 3 :startblock -1 :blocktime 0 :state 1))
(defparameter *process-2*
  '(:id 2  :priority 30 :cputime 0 :alltime 6 :startblock -1 :blocktime 0 :state 1))
(defparameter *process-3*
  '(:id 3  :priority 29 :cputime 0 :alltime 3 :startblock -1 :blocktime 0 :state 1)) 
(defparameter *process-4*
  '(:id 4 :priority 0 :cputime 0 :alltime 8 :startblock -1 :blocktime 0 :state 1))

(defparameter *process-list* `(,*process-0* ,*process-1*,*process-2*,*process-3*,*process-4*))
(defparameter *priority-list* '())          ;定义一个优先级进程队列列表
(defparameter *block-list*'())         ;定义一个阻塞进程列表
(defparameter *machine-time* 1)        ;设置时间片长度
(defparameter *count* 0)               ;记录系统运行的时间片
(defparameter *block-time-flag* 0)     ;阻塞进程剩余时间记录
(defparameter *max-priority* 0)        ;所有进程的最大优先数


(defun get-max-priority()            ;查找所有进程的最大优先，
  (let ((var ()))
    (dolist (lst *process-list*)
      (if(> (getf lst :alltime) 0)
	 (push (getf lst :priority) var)))
    (setf *max-priority* (car(sort var #'>)))))       ;将最大优先数传递给变量 *max-priority*

(defun range-process()                 ;以优先数排列排列进程，并放到优先级进程队列列表
  (get-max-priority)
  (if (= (length *process-list*) 0)
      nil
      (progn
	(dolist (lst *process-list*)
	  (when (= (getf lst :priority) *max-priority*)
	    (push lst *priority-list*)
	    (setf *process-list* (remove lst *process-list*))))
	(range-process))))

(defun transfer-list()              ;列表传递
  (range-process)
  (setf *priority-list* (reverse *priority-list*)))

(defun increase-reduce-property(lst)  ;执行一个时间片，改变所有进程的优先数，已运行时间，还需分配时间
  (if (> (length *priority-list*) 1)
      (dolist (lst1 *priority-list*)
	(if (equalp lst lst1)
	    (progn
	      (decf (getf lst1 :priority) 3)
	      (decf (getf lst1 :alltime) *machine-time*)
	      (incf (getf lst1 :cputime) *machine-time*))
	    (incf (getf lst1 :priority) 1)))
      (progn
	(decf (getf (car *priority-list*) :priority) 3)
	(decf (getf (car *priority-list*) :alltime) *machine-time*)
	(incf (getf (car  *priority-list*) :cputime) *machine-time*))))

(defun show-block-list()                      ;显示阻塞进程
  (if (> (length *block-list*) 0)
      (progn
	(format t "block-list~%")
	(format t " ~a ~%~%" *block-list*))
      (format T "block-list : nil~%")))

(transfer-list)

(defun deal-block-process()
  (when  (> (length *block-list*) 0)               ;如果阻塞进程不为空，将对阻塞进程进行处理
    (decf (getf (car *block-list*) :blocktime) *machine-time*)
    (when (<= (getf (car *block-list*) :blocktime) 0)               ;当阻塞进程的阻塞时间已完，
      (setf (getf (car *block-list*) :startblock) -1)
      (setf (getf (car *block-list*) :state) 1)
      (setf *block-time-flag* 0)
      (setf *priority-list* (append *priority-list*  *block-list*))  ;将该阻塞进程调入就绪进程
      (setf *block-list* ()))))

(defun show-priority-list()                            ;显示优先进程队列里的所有进程
  (if (> (length *priority-list*) 0)
      (progn (format t  "ready-list:~%")
	     (dolist (lst *priority-list*)
	       (format t " ~a ~%" lst))
	     (format t "~%"))))

(defun run-process()
  (if  (> (length *priority-list*) 0)                              ;递归，当优先级队列非空时
       (if  (= (getf (car *priority-list*) :startblock) -1)     ;判断该进程是否为阻塞进程
	    (progn                                             ;如果不是阻塞进程，执行
	      (increase-reduce-property (car *priority-list*))          ;改变所有进程的属性
	      (when (<= (getf (car *priority-list*) :alltime) 0)              ;移除执行完毕的进程
		(setf *priority-list* (remove (car *priority-list*) *priority-list*)))
	      (format t " running-process-id : ~a ~%" (getf (car *priority-list*) :id))
	      (show-priority-list)
	      (show-block-list)
	      (deal-block-process)                                      ;处理阻塞进程
	      (incf *count*)
	      (setf *process-list* *priority-list*)                     
	      (setf *priority-list*())
	      (sleep 0.8)
	      (transfer-list)
	      (run-process))
	    (progn
	      (when(> (length *priority-list*) 0)
		(push (car *priority-list*) *block-list*)
		(setf *priority-list* (remove (car *priority-list*) *priority-list*))
		(setf *block-time-flag* (getf (car *block-list*) :startblock))  ;取出阻塞时间
		(setf (getf (car *block-list*) :state) 0))
	      (increase-reduce-property (car *priority-list*))
	      (when (<= (getf (car *priority-list*) :alltime) 0)              ;移除执行完毕的进程
		(setf *priority-list* (remove (car *priority-list*) *priority-list*)))
	      (format t " running-process-id : ~a ~%" (getf (car *priority-list*) :id))
	      (show-priority-list)
	      (show-block-list)
	      (deal-block-process)
	      (setf *process-list* *priority-list*)
	      (setf *priority-list*())
	      (sleep 0.8)
	      (transfer-list)
	      (run-process)))))










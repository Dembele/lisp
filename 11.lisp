(defun t11 (input-list n)
	(if input-list
		(if (zerop n)
			(cons nil (cons input-list nil))
   ((lambda (elem result)
     (cons
      (cons elem (car result))
      (cdr result)))
    (car input-list)
    (t11 (cdr input-list) (1- n)))))
)

(defun fibonacci(n)
  (cond
    ((eq n 0) 0)
    ((eq n 1) 1)
    ( (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(with-open-file (str "text0.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	(loop for a from 0 to 40	
		do (format str (write-to-string(fibonacci a)))
	 	(format str "~%" )) )


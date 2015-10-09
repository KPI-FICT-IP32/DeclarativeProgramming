(load 'funclib.lsp)
(defun sieve (n)
  (cond 
    ((< n 2) nil)
    (t (labels
         ((sieve-step (all primes) 
                      (cond
                        ((null all) primes)
                        (t (sieve-step
                             (remove-if (lambda (x) (= 0 (rem x (car all)))) all)
                             (append primes (list (car all))))))))
         (sieve-step (range 2 (+ n 1)) nil)))))

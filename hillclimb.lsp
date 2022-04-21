(setq graph '( 
               (a (b 3) (c 1)) 
               (b (a 3) (d 1)) 
               (c (a 1) (d 2) (e 2)) 
               (d (b 1) (c 2) (e 1) (g 2)) 
               (e (c 2) (d 1) (f 3)) 
               (f (e 3) (g 1)) 
               (g (d 2) (f 1))) 
)

(defun member (A L)
    (cond
        ((NULL L) '())
        ((EQ (car L) A) T)
        (T (member A (cdr L)))))
        
(defun length (L)
    (do ((M L)(sum 0))
        ((NULL M) sum)
        (setq M (cdr M))
        (setq sum(+ sum 1))))

(defun cdrList (start graph)
    (cond
        ((> (length graph) 2)
        (cond
            ((eq (car(car graph)) start) graph)
            ((cdrList start (cdr graph))))
        (cdr graph))))

(defun hillClimb (start target graph &optional path)
    (cond
        ((eq start target)
        (push start path)
        (write (nreverse path)))
        ((eq (car(car graph)) start)
        (push start path)
        (setq start (expandNode start target (cdr(car graph)) path))
        (hillClimb start target (cdrList start graph) path))
        ((hillClimb start target (cdrList start graph) path))))

(defun expandNode (node target connections path)
    (setq shortest 9999)
    (do ((M connections))
        ((NULL M) nextNode)
        (cond
            ((member target (car M))
            (setq nextNode target)
            (setq M (cdr M)))
            ((member (car(car M)) path) (setq M (cdr M)))
            ((cond
                ((< (car(cdr(car M))) shortest)
                (setq shortest (car(cdr(car M))))
                (setq nextNode (car(car M)))
                (setq M (cdr M)))
                ((setq M (cdr M))))))))

(hillClimb 'a 'g graph)
(setq graph '( 
               (a (b 3) (c 1)) 
               (b (a 3) (d 1)) 
               (c (a 1) (d 2) (e 2)) 
               (d (b 1) (c 2) (e 1) (g 2)) 
               (e (c 2) (d 1) (f 3)) 
               (f (e 3) (g 1)) 
               (g (d 2) (f 1))) 
)

(defun memberb (A L)
    (cond
        ((NULL L) '())
        ((EQ (car L) A) T)
        (T (memberb A (cdr L)))))

(defun reverseL (L)
    (cond
        ((null L) `())
        ((null (cdr L)) (list (car L)))
        (T (append (reverseL(cdr L)) (list (car L))))
    )
)
        
(defun lengthb (L)
    (do ((M L)(sum 0))
        ((NULL M) sum)
        (setq M (cdr M))
        (setq sum(+ sum 1))))

(defun cdrList (start graph)
    (cond
        ((> (lengthb graph) 2)
        (cond
            ((eq (car(car graph)) start) graph)
            ((cdrList start (cdr graph))))
        (cdr graph))))

(defun dfs (start target graph &optional path)
    ; Sets each node that was visited in the graph
    (setq visited '())
    (cond
        ; If the start value equals the target prints out the final path (null / recursive case ex. a -> a)
        ((eq start target) (push start path) (write (reverseL path)))
    )
        ;Dfs loops though the graph until the specific start path is found
        (setq path '())
        (push start path)
        (push start visited)
        ;(print (cdr (car M)))
        
        (dfsR start target (findPath start graph) graph visited path `())
        

        ;(setq start (expandNode start target (cdr(car graph)) path))
)

(defun dfsR (start target newGraph graph visited path optimalPath)
    ; Iterates though each connection in each path recurisvely until one finds the goal
    (do ((M newGraph (cdr M)))
        ((NULL M) optimalPath)
        (cond
            ((eq target (car (car M))) 
            (setq path (append (list(car (car M))) path))
            (cond
                ((eq optimalPath nil) (setq optimalPath path))
                ((> (length optimalPath) (length path)) (setq optimalPath path))
            )
            (reverseL optimalPath))
            ((not (memberb (car (car M)) visited))
            (setq visited (append (list(car (car M))) visited))
            (setq path (append (list(car (car M))) path))
            ;(print (car M))
            ;(print visited)
            ;(print (reverseL path))
            (setq optimalPath (dfsR start target (findPath (car (car M)) graph) graph visited path optimalPath))
            )
        )
    )
)

(defun findPath (A graph)
    (cond
        ((eq A (car (car graph))) (cdr (car graph)))
        (T (findPath A (cdr graph)))
    )
)

(defun hillClimb (start target graph &optional path)
    (cond
        ((eq start target) (push start path) (write (nreverse path)))

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
            ((memberb target (car M))
            (setq nextNode target)
            (setq M (cdr M)))
            ((memberb (car(car M)) path) (setq M (cdr M)))
            ((cond
                ((< (car(cdr(car M))) shortest)
                (setq shortest (car(cdr(car M))))
                (setq nextNode (car(car M)))
                (setq M (cdr M)))
                ((setq M (cdr M))))))))
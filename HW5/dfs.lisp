;;Depth First Search Algorithm
;;dfs performs a depth-first search on one pass though the graph and
;;dfsOP finds the optimal path by checking every option

(setq graph '( 
               (a (b 3) (c 1)) 
               (b (a 3) (d 1)) 
               (c (a 1) (d 2) (e 2)) 
               (d (b 1) (c 2) (e 1) (g 2)) 
               (e (c 2) (d 1) (f 3)) 
               (f (e 3) (g 1)) 
               (g (d 2) (f 1))) 
)

; checks if atom A is a member of list L
(defun memberb (A L)
    (cond
        ((NULL L) '())
        ((EQ (car L) A) T)
        (T (memberb A (cdr L)))))

; returns the items in the list in reverse order
(defun reverseL (L)
    (cond
        ((null L) `())
        ((null (cdr L)) (list (car L)))
        (T (append (reverseL(cdr L)) (list (car L))))
    )
)

; iteratively finds length of the list
(defun lengthb (L)
    (do ((M L)(sum 0))
        ((NULL M) sum)
        (setq M (cdr M))
        (setq sum(+ sum 1))))

; grabs the cdr of a list starting at element start
(defun cdrList (start graph)
    (cond
        ((> (lengthb graph) 2)
        (cond
            ((eq (car(car graph)) start) graph)
            ((cdrList start (cdr graph))))
        (cdr graph))))

; Conducts a depth first search on the graph
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
        
        ; Recursively runs dfs until a solution is found
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

                ; Used to find the absoulte optimal path using dfs
                ; ((> (length optimalPath) (length path)) (setq optimalPath path))
            )
            (reverseL optimalPath))
            ((not (memberb (car (car M)) visited))
            (setq visited (append (list(car (car M))) visited))
            (setq path (append (list(car (car M))) path))
            (setq optimalPath (dfsR start target (findPath (car (car M)) graph) graph visited path optimalPath))
            )
        )
    )
)

; Finds the specific adjacency list row for each element in graph
(defun findPath (A graph)
    (cond
        ((eq A (car (car graph))) (cdr (car graph)))
        (T (findPath A (cdr graph)))
    )
)

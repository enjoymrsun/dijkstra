;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment9-Rob) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Node is a Symbol 
;; INTERP: represents the name of a node in a graph

;; A Distance is a PosInt
;; INTERP: represents distance in miles

;; An Edge is (list Node Distance Node)
;; e.g. (list 'A 10 'B)
;; INTERP: represents an edge from 'A to 'B with the distance from 'A to 'B being 10 miles

;; A Path is a [List-of Edge]
;; A Graph is a [Set-of Edge]
;; NOTE: you can use the definition of Set from your previous assignment.

;; Valid Edges:
(define e1 (list 'e 35 'f))
(define e2 (list 'f 35 'e))
(define e3 (list 'e 37 'h))
(define e4 (list 'f 28 'h))
(define e5 (list 'h 28 'f))
(define e6 (list 'f 32 'b))
(define e7 (list 'a 38 'e))
(define e8 (list 'a 26 'c))
(define e9 (list 'h 39 'd))
(define e10 (list 'b 29 'd))
(define e11 (list 'c 34 'h))
(define e12 (list 'g 40 'c))
(define e13 (list 'd 52 'g))
(define e14 (list 'g 58 'a))
(define e15 (list 'g 93 'e))
(define e18 (list 'i 48 'd))
(define e19 (list 'e 25 'g))
(define e20 (list 'i 10 'b))
(define e21 (list 'i 108 'g))
(define e22 (list 'c 120 'd))
;; Invalid Edges (used for problem 1 and 2):
(define e16 (list 'a 31 'h))
(define e17 (list 'h 13 'g))
;; Extra Edges (used for problem 4):
(define e23 (list 'f 22 'j))
(define e24 (list 'b 30 'j))

(define g0 '())  ;; empty graph
;; a valid graph
(define g1 (list e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e18 e19 e20 e21 e22))
;; an extra graph
(define g2 (list e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e18 e19 e20 e21 e22 e23 e24))

(define path0 '())  ;; empty path
(define path1 (list e8 e11 e9))  ;; valid path
(define path2 (list e8 e11 e3))  ;; invalid path
(define path3 (list e16))  ;; invalid path
(define path4 (list e16 e17))  ;; invalid path
(define path5 (list e10))  ;; valid path



;; Problem 1
;; Solutions:
;; use andmap to examine each Edge in the [List-of Edge] are all exist in the [Set-of Edge] and
;; examine each Edge (without last Edge) in the [List-of Edge]'s end node
;; is same with next Edge's start node

;; Main Function:
;; valid-path?: [Set-of Edge] [List-of Edge] -> Boolean
;; consumes a graph and a path, and
;; returns true if the path is valid for the graph, and false otherwise
;; A path is valid for a graph if
;; it is possible to follow each edge in order from the path on the graph
(check-expect (valid-path? g0 path0) #true)  ;; empty Graph with empty Path
(check-expect (valid-path? g0 path2) #false)  ;; empty Graph with invalid path
(check-expect (valid-path? g0 path1) #false)  ;; empty Graph with valid path
(check-expect (valid-path? g1 path0) #true)  ;; empty Path
(check-expect (valid-path? g1 path1) #true)
(check-expect (valid-path? g1 path2) #false)  ;; invalid path (Edges are not connected)
(check-expect (valid-path? g1 path3) #false)  ;; invalid path (Edge does not exist in the Graph)
(check-expect (valid-path? g1 path4) #false)  ;; invalid path (Edges do not exist in the Graph)
(check-expect (valid-path? g1 path5) #true)
(define (valid-path? graph path)
  (and (andmap (lambda(x) (contain-the-edge? x graph)) path)
       (valid-edge-sequence? path)))

;; Helper Functions:
;; 1.
;; equal-edge?: Edge Edge -> Boolean
;; given two valid Edges, determine whether they are completely identical
(check-expect (equal-edge? e6 e6) #true)
(check-expect (equal-edge? (list 'a 20 'b) (list 'a 25 'b)) #false)  ;; wrong Distance
(check-expect (equal-edge? (list 'a 20 'b) (list 'c 20 'b)) #false)  ;; wrong start node
(check-expect (equal-edge? (list 'a 20 'b) (list 'a 20 'c)) #false)  ;; wrong end node
(define (equal-edge? e1 e2)
  (and (symbol=? (first e1) (first e2))
       (= (second e1) (second e2))
       (symbol=? (third e1) (third e2))))

;; 2.
;; contain-the-edge?: Edge [Set-of Edge] -> Boolean
;; given an Edge and a [Set-of Edge], determine whether the Edge exists in this Set of Edges
(check-expect (contain-the-edge? e10 g1) #true)
(check-expect (contain-the-edge? e18 g1) #true)
(check-expect (contain-the-edge? e16 g1) #false)  ;; Edge not exist in this non empty graph
(check-expect (contain-the-edge? e10 g0) #false)  ;; empty Graph
(check-expect (contain-the-edge? e16 g0) #false)  ;; empty Graph
(define (contain-the-edge? e g)
  (ormap (lambda(x) (equal-edge? e x)) g))

;; 3.
;; valid-edge-sequence?: [List-of Edge] -> Boolean
;; determine whether each Edge (without last Edge) in the [List-of Edge]'s end Node
;; is same with next Edge's start Node
;; empty list of edge return true
(check-expect (valid-edge-sequence? '()) #true)
(check-expect (valid-edge-sequence? path1) #true)
(check-expect (valid-edge-sequence? path2) #false)
(check-expect (valid-edge-sequence? path3) #true)
(check-expect (valid-edge-sequence? path4) #true)
(check-expect (valid-edge-sequence? path5) #true)
(check-expect (valid-edge-sequence? (list e4 e10 e12)) #false) 
(define (valid-edge-sequence? path)
  (cond [(empty? path) #true]
        [(= 1 (length path)) #true]
        [(= 2 (length path)) (connected-edges? (first path)
                                               (second path))]
        [else (and (connected-edges? (first path)
                                     (second path))
                   (valid-edge-sequence? (cdr path)))]))

;; 4.
;; connected-edges?: Edge Edge -> Boolean
;; determine whether the first edge's end Node is same with second Edge's start Node
;; inputs should not be invalid Edges
(check-expect (connected-edges? e6 e10) #true)
(check-expect (connected-edges? e13 e12) #true)
(check-expect (connected-edges? e10 e6) #false)
(check-expect (connected-edges? e3 e11) #false)
(define (connected-edges? e1 e2)
  (symbol=? (third e1) (first e2)))


         
;; Problem 2
;; Solutions:
;; examine whether the begin Node in first Edge of the [List-of Edge]
;; is same with the start Node s and
;; exam the path is valid for the graph and
;; examine whether the end Node in last Edge of the [List-of Edge] is same with the end Node t

;; Main Function:
;; valid-st-path?: [Set-of Edge] Symbol Symbol [List-of Edge] -> Boolean
;; consumes a graph g, a start Node s, an end Node t, and a path p,
;; and returns true if starting at Node s in g and following, in order,
;; the edges in p, will lead us to t. return false otherwise.
(check-expect (valid-st-path? g1 'a 'd path1) #true)
(check-expect (valid-st-path? g1 'a 'e (list e8 e22 e13 e15)) #true)
(check-expect (valid-st-path? g1 'a 'a '()) #true)  ;; same start node and end node
(check-expect (valid-st-path? g1 'f 'a (list e2 e19)) #false) ;; wrong end Node in the path
(check-expect (valid-st-path? g1 'e 'h (list e15 e3)) #false) ;; wrong start Node in the path
(check-expect (valid-st-path? g1 'f 'g (list e6 e20 e21)) #false)
;; an invalid path (Edges are not connected)
(check-expect (valid-st-path? g0 'a 'e (list e8 e22 e13 e15)) #false)  ;; empty Graph
(define (valid-st-path? graph s t path)
  (or (and (empty? path)
           (symbol=? s t))
      (and (correct-start-node? s path)
           (valid-path? graph path)
           (correct-end-node? t path))))

;; Helper Functions:
;; 1.
;; correct-begin-Node?: Symbol NonEmpty[List-of Edge] -> Boolean
;; whether the path begins with the correct Node
(check-expect (correct-start-node? 'e (list e15 e3)) #false)
(check-expect (correct-start-node? 'g (list e15 e3)) #true)
(define (correct-start-node? s path)
  (symbol=? s
            (caar path)))

;; 2.
;; correct-end-Node?: Symbol NonEmpty[List-of Edge] -> Boolean
;; whether the path ends with the correct Node
(check-expect (correct-end-node? 'a (list e2 e19)) #false)
(check-expect (correct-end-node? 'g (list e2 e19)) #true)
(define (correct-end-node? t path)
  (symbol=? t
            (third (last-in-list path))))

;; 3.
;; last-in-list: NonEmpty[List-of X] -> X
;; return the last element in a list of X
(check-expect (last-in-list (list 'a 123 "sfaf" 'af)) 'af)
(check-expect (last-in-list (list 'a 123 "sfaf")) "sfaf")
(check-expect (last-in-list (list 'a 123 "sfaf" 'af 42)) 42)
(check-expect (last-in-list (list "fighting")) "fighting")  ;; only one element in the list
(define (last-in-list alist)
  (cond [(null? (cdr alist)) (car alist)]
        [else (last-in-list (cdr alist))]))



;; Problem 3
;; Solutions:
;; depth-first-search

;; Main Function:
;; find-st-path: [Set-of Edge] Symbol Symbol -> [List-of Edge]/#false
;; consumes a graph g, a start node s and an end node t and
;; returns a path p that starts at s and ends at t in the graph g
;; return false if there is no such path
(check-expect (find-st-path g1 'c 'g)
              (list (list 'c 34 'h) (list 'h 28 'f) (list 'f 35 'e) (list 'e 25 'g)))
(check-expect (find-st-path g1 'e 'g) (list (list 'e 25 'g)))  ;; one edge path
(check-expect (find-st-path g1 'h 'a)
              (list (list 'h 28 'f) (list 'f 35 'e) (list 'e 25 'g) (list 'g 58 'a)))
(check-expect (find-st-path g1 'g 'f)
              (list (list 'g 40 'c) (list 'c 34 'h) (list 'h 28 'f)))
(check-expect (find-st-path g1 'c 'e)
              (list (list 'c 34 'h) (list 'h 28 'f) (list 'f 35 'e)))
(check-expect (find-st-path g1 'e 'i) #false)  ;; valid Nodes but no such path
(check-expect (find-st-path g1 'a 'i) #false)  ;; valid Nodes but no such path
(check-expect (find-st-path g1 'a 'z) #false)  ;; invalid end Node 'z in Graph
(check-expect (find-st-path g1 'w 'b) #false)  ;; invalid start Node 'w in Graph
(check-expect (find-st-path g1 'w 'z) #false)
;; invalid start Node 'w and invalid end Node 'z in Graph
(check-expect (find-st-path g0 'a 'b) #false)  ;; empty Graph
(check-expect (find-st-path g0 'f 'g) #false)  ;; empty Graph
(define (find-st-path graph s t)
  (cond [(symbol=? s t) '()]
        [else (local [(define anEdge (find-st-edge graph s t))]
                (cond [(cons? anEdge) (list anEdge)]
                      [else (local [(define nears (neighbors graph s))
                                    (define aPath
                                      (find-manyst-path
                                       (remove-seen-edges graph s nears) nears t))]
                              (cond [(cons? aPath) (cons (find-st-edge graph s (caar aPath))
                                                         aPath)]
                                    [else #false]))]))]))
                                                                       
;; Helper Functions:
;; 1.
;; valid-edge?: Edge Symbol Symbol -> Boolean
;; given an Edge e, a start node s and an end node t,
;; returns true when the edge starts at s and ends at t,
;; otherwise return false;
(check-expect (valid-edge? e4 'f 'h) #true)
(check-expect (valid-edge? e6 'f 'b) #true)
(check-expect (valid-edge? e8 'g 'c) #false) ;; wrong start node
(check-expect (valid-edge? e8 'a 'b) #false) ;; wrong end node
(check-expect (valid-edge? '() 'a 'b) #false)  ;; empty Edge
(define (valid-edge? e s t)
  (if (empty? e)
      #false
      (and (symbol=? (first e) s)
           (symbol=? (third e) t))))

;; 2.
;; find-st-edge: [Set-of Edge] Symbol Symbol -> Edge/#false
;; consumes a graph g, a start node s and an end node t and
;; returns one edge e that starts at s and ends at t in the graph g
;; return false if there is no such an edge
(check-expect (find-st-edge g1 'i 'b) e20)
(check-expect (find-st-edge g1 'i 'g) e21)
(check-expect (find-st-edge g1 'h 'g) #false)
(check-expect (find-st-edge g1 'b 'h) #false)
(check-expect (find-st-edge g0 'i 'b) #false)  ;; empty Graph
(define (find-st-edge g s t)
  (local [(define l (filter (lambda(x) (valid-edge? x s t)) g))]
    (cond [(empty? l) #false]
          [else (first l)])))

;; 3.
;; neighbors: [Set-of Edge] Symbol -> [List-of Symbol]
;; return a list of Nodes which are the neighbors
;; for the given start Node with only one edge distance
(check-expect (neighbors g1 'h) (list 'f 'd))
(check-expect (neighbors g1 'g) (list 'c 'a 'e))
(check-expect (neighbors g1 'f) (list 'e 'h 'b))
(check-expect (neighbors g2 'j) '())  ;; Node exists in Graph but no outgoing neighbors
(check-expect (neighbors g0 'a) '())  ;; empty Graph
(check-expect (neighbors g2 'w) '())  ;; Node not exists in Graph, no outgoing neighbors
(define (neighbors graph s)
  (map (lambda(x) (caddr x))
       (filter (lambda(x) (if (empty? x)
                              #false
                              (symbol=? (first x) s))) graph)))

;; 4.
;; find-manyst-path: [Set-of Edge] [List-of Symbol] Symbol -> [List-of Edge]/#false
;; finds a valid path from a list of start Nodes to the end Node t in the given graph
;; return false if there is no valid path from all the given start Nodes to the given end Node
(check-expect (find-manyst-path g1 (list 'e 'c) 'b) (list (list 'e 35 'f) (list 'f 32 'b)))
(check-expect (find-manyst-path g1 (list 'e 'a 'c) 'i) #false)
(check-expect (find-manyst-path g0 (list 'e 'a 'c) 'i) #false)  ;; empty Graph
(check-expect (find-manyst-path g1 '() 'i) #false)  ;; empty list of start Nodes
(check-expect (find-manyst-path g1 (list 'e 'a 'c) 'z) #false)  ;; invalid end node
(check-expect (find-manyst-path g1
                                (list 'f 'd) 'a) (list (list 'f 35 'e)
                                                       (list 'e 37 'h) (list 'h 39 'd)
                                                       (list 'd 52 'g) (list 'g 58 'a)))
(define (find-manyst-path graph alos t)
  (cond [(empty? graph) #false]
        [(empty? alos) #false]
        [else (local [(define aPath (find-st-path graph (first alos) t))]
                (cond [(cons? aPath) aPath]
                      [else (find-manyst-path graph (rest alos) t)]))]))

;; 5.
;; remove-seen-edges: [Set-of Edge] Symbol [List-of Symbol] -> [Set-of Edge]
;; remove the edges that you already traversed in a graph
;; first Symbol means the start Node, a list of Symbol is a list of end Node
;; remove the edges that start from the start node and ends in the list of end Note
(check-expect (remove-seen-edges g1 'a '(c e))
              (list (list 'e 35 'f) (list 'f 35 'e) (list 'e 37 'h) (list 'f 28 'h) (list 'h 28 'f)
                    (list 'f 32 'b) (list 'h 39 'd) (list 'b 29 'd) (list 'c 34 'h) (list 'g 40 'c)
                    (list 'd 52 'g) (list 'g 58 'a) (list 'g 93 'e) (list 'i 48 'd) (list 'e 25 'g)
                    (list 'i 10 'b) (list 'i 108 'g) (list 'c 120 'd)))
(check-expect (remove-seen-edges g1 'g '(e a c))
              (list (list 'e 35 'f) (list 'f 35 'e) (list 'e 37 'h) (list 'f 28 'h) (list 'h 28 'f)
                    (list 'f 32 'b) (list 'a 38 'e) (list 'a 26 'c) (list 'h 39 'd) (list 'b 29 'd)
                    (list 'c 34 'h) (list 'd 52 'g) (list 'i 48 'd) (list 'e 25 'g) (list 'i 10 'b)
                    (list 'i 108 'g) (list 'c 120 'd)))
(check-expect (remove-seen-edges (list e7 e8) 'a '(c e)) '())  ;; Graph becomes empty
(check-expect (remove-seen-edges '() 'a '(c e)) '())  ;; empty Graph
(check-expect (remove-seen-edges (list e7 e8) 'x '(c e)) (list e7 e8))  ;; invalid start Node
(check-expect (remove-seen-edges (list e7 e8) 'a '()) (list e7 e8))  ;; empty list of End nodes
(define (remove-seen-edges g s alot)
  (filter (lambda(x) (if (empty? x)
                         #false
                         (if (and (symbol=? (first x) s) (member? (third x) alot))
                             #false
                             #true))) g)) 

;; Problem 4
;; solutions:
;; Dijkstra Algorithm

; A Complete-Graph is a (make-cg [List-of Symbol] [List-of Edge] [List-of Number] [List-of Symbol])
;; interpretation:
;; nodes represent the distinct Nodes in the Graph
;; edges represent each node's the last Edge in the path from a start Node to itself
;; costs represent each node's distance from a start node to itself
;; nodes-visited represent the nodes that its distance
;; from the start node to itself is already minimum and don't need to calculate again
;; these four instance variables are order matters,
;; each represent a node's relation with the start Node
(define-struct cg (nodes edges costs nodes-visited))

;; Main Function:
;; find-shortest-distance-st-path: [Set-of Edge] Symbol Symbol -> [List-of Edge]/#false
;; consumes a graph g, a start node s and an end node t and
;; returns the path p that starts at s ends at t in the graph g and
;; when we add the distances of each edge in p that is the shortest distance from s to t in g
;; return false if there is no such path
(check-expect (find-shortest-distance-st-path g1 'f 'd) (list (list 'f 32 'b) (list 'b 29 'd)))
(check-expect (find-shortest-distance-st-path g1 'a 'h) (list (list 'a 26 'c) (list 'c 34 'h)))
(check-expect (find-shortest-distance-st-path g0 'a 'd) #false)
;; empty graph, no path from 'a to 'd
(check-expect (find-shortest-distance-st-path g1 'a 'i) #false)
;; no path from 'a to 'i
(check-expect (find-shortest-distance-st-path g1 'z 'i) #false)
;; no Node called 'z in the graph, wrong start Node
(check-expect (find-shortest-distance-st-path g1 'i 'w) #false)
;; no Node called 'w in the graph, wrong end Node
(check-expect (find-shortest-distance-st-path g1 'x 'y) #false)
;; no Node called 'x 'y in the graph, wrong start Node and wrong end Node
(check-expect (find-shortest-distance-st-path g1 'i 'g)
              (list (list 'i 10 'b) (list 'b 29 'd) (list 'd 52 'g)))
(check-expect (find-shortest-distance-st-path g1 'f 'g) (list (list 'f 35 'e) (list 'e 25 'g)))
(check-expect (find-shortest-distance-st-path g1 'f 'c)
              (list (list 'f 35 'e) (list 'e 25 'g) (list 'g 40 'c)))
(check-expect (find-shortest-distance-st-path g1 'h 'a)
              (list (list 'h 28 'f) (list 'f 35 'e) (list 'e 25 'g) (list 'g 58 'a)))
(check-expect (find-shortest-distance-st-path g1 'a 'd)
              (list (list 'a 26 'c) (list 'c 34 'h) (list 'h 39 'd)))
(check-expect (find-shortest-distance-st-path g1 'c 'd) (list (list 'c 34 'h) (list 'h 39 'd)))
(check-expect (find-shortest-distance-st-path g1 'c 'g)
              (list (list 'c 34 'h) (list 'h 28 'f) (list 'f 35 'e) (list 'e 25 'g)))
(check-expect (find-shortest-distance-st-path g2 'g 'h)
              (list (list 'g 40 'c) (list 'c 34 'h)))
(define (find-shortest-distance-st-path graph s t)
  (cond [(symbol=? s t) '()]
        [(not (member? s (remove-duplicate-nodes (nodes-in-graph graph)))) #false]
        [(not (member? t (remove-duplicate-nodes (nodes-in-graph graph)))) #false]
        [else
         (local [;; make-a-cg: [Set-of Edge] Symbol -> Complete-Graph
                 ;; given a Graph, return a Complete-Graph
                 ;; according to the data definitions for the Complete-Graph
                 ;; Symbol s is the start Node 
                 (define (make-a-cg graph s)
                   (local [(define nodes (remove-duplicate-nodes (nodes-in-graph graph)))
                           (define edges (make-list (length nodes) '()))
                           (define costs (change-in-list (make-list (length nodes) +inf.0)
                                                         (node-index nodes s) 0))
                           (define visited-nodes '())]
                     (make-cg nodes edges costs visited-nodes)))
                 ;; relax-edges: Complete-Graph [List-of Edge] -> [List-of Edge]
                 ;; given a Complete-Graph, and a list of Edges to relax,
                 ;; return a new list of Edges with possible less path distance
                 ;; compared with before
                 (define (relax-edges cg aloe)
                   (cond [(empty? aloe) (cg-edges cg)]
                         [else (local [(define startnode (first (first aloe)))
                                       (define endnodes (map (lambda(x) (third x)) aloe))
                                       ;; relax-edges-with-acc:
                                       ;; NonNegInteger [List-of Edge] -> [List-of Edge]
                                       ;; given a NonNegInteger represent the index in this list of Edge
                                       ;; you are currently looking at
                                       ;; and a list of Edges represents the Edges you need to relax
                                       ;; return the already relaxed list of Edges
                                       ;; idx will add 1 after we finish this Node's
                                       ;; distance comparison
                                       (define (relax-edges-with-acc idx anewloe)
                                         (cond [(= idx (length (cg-nodes cg))) anewloe]
                                               [else (local [;; n means the node we are
                                                             ;; looking at, as the end node of an edge
                                                             (define n (list-ref (cg-nodes cg) idx))]
                                                       (if (member? n endnodes)
                                                           (local [;; an edge maybe helps to reduce the cost
                                                                   ;; from startnode to node n
                                                                   (define e (list-ref aloe (node-index endnodes n)))
                                                                   (define newcost (+ (list-ref (cg-costs cg)
                                                                                                (node-index (cg-nodes cg) startnode))
                                                                                      (second e)))]
                                                             (if (> (list-ref (cg-costs cg) idx) newcost)
                                                                 (cons e (relax-edges-with-acc (add1 idx)
                                                                                               (rest anewloe)))
                                                                 (cons (first anewloe) (relax-edges-with-acc (add1 idx)
                                                                                                             (rest anewloe)))))
                                                           (cons (first anewloe) (relax-edges-with-acc (add1 idx)
                                                                                                       (rest anewloe)))))]))]
                                 (relax-edges-with-acc 0 (cg-edges cg)))]))
                 ;; relax-costs: Complete-Graph [List-of Edge] -> [List-of Number]
                 ;; given a Complete-Graph, and a list of Edges to relax,
                 ;; return a new list of Number with possible less path distance
                 ;; compared with before
                 (define (relax-costs cg aloe)
                   (cond [(empty? aloe) (cg-costs cg)]
                         [else (local [(define startnode (first (first aloe)))
                                       (define endnodes (map (lambda(x) (third x)) aloe))
                                       ;; relax-costs-with-acc:
                                       ;; NonNegInteger [List-of Number] -> [List-of Number]
                                       ;; given a NonNegInteger represent the
                                       ;; index in the list of Nodes
                                       ;; you are currently looking at
                                       ;; and a list of number represents the costs from start
                                       ;; node to each node, return the already relaxed
                                       ;; list of number
                                       ;; idx will add 1 after we finish this Node's
                                       ;; distance comparison
                                       (define (relax-costs-with-acc idx alon)
                                         (cond [(= idx (length (cg-nodes cg))) alon]
                                               [else (local [;; n means the node we are
                                                             ;; looking at, as the end node of a edge
                                                             (define n (list-ref (cg-nodes cg) idx))]
                                                       (if (member? n endnodes)
                                                           (local [;; an edge maybe helps to reduce
                                                                   ;; the cost from startnode to node n
                                                                   (define e (list-ref aloe (node-index endnodes n)))
                                                                   (define newcost (+ (list-ref (cg-costs cg)
                                                                                                (node-index (cg-nodes cg) startnode))
                                                                                      (second e)))]
                                                             (if (> (list-ref (cg-costs cg) idx) newcost)
                                                                 (cons newcost (relax-costs-with-acc (add1 idx)
                                                                                                     (rest alon)))
                                                                 (cons (first alon) (relax-costs-with-acc (add1 idx)
                                                                                                          (rest alon)))))
                                                           (cons (first alon) (relax-costs-with-acc (add1 idx)
                                                                                                    (rest alon)))))]))]
                                 (relax-costs-with-acc 0 (cg-costs cg)))]))
                 ;; find-shortest-p: Complete-Graph -> Complete-Graph
                 ;; given a Complete-Graph, keep relax its Nodes, distances and Edges
                 ;; until all Nodes, distances and Edges are already relaxed
                 (define (find-shortest-p acg)
                   (local [(define n (minimum-path-without-except
                                      (cg-costs acg) (cg-nodes acg)
                                      (cg-nodes-visited acg)))]
                     (if (boolean? n)
                         acg
                         (find-shortest-p (make-cg (cg-nodes acg)
                                                   (relax-edges acg
                                                                (edges-from-a-start-node graph n))
                                                   (relax-costs acg
                                                                (edges-from-a-start-node graph n))
                                                   (cons n (cg-nodes-visited acg)))))))]
           (find-st-path (cg-edges (find-shortest-p (make-a-cg graph s))) s t))]))
        
;; Helper Functions:
;; 1.
;; edges-from-a-start-node: [Set-of Edge] Symbol -> [List-of Edge]
;; given a graph and a start node,
;; return a list composed of all the single edges that begin from the start node
(check-expect (edges-from-a-start-node g1 'a) (list (list 'a 38 'e) (list 'a 26 'c)))
(check-expect (edges-from-a-start-node g1 'g)
              (list (list 'g 40 'c) (list 'g 58 'a) (list 'g 93 'e)))
(check-expect (edges-from-a-start-node g0 'a) '())  ;; empty Graph
(check-expect (edges-from-a-start-node g1 'v) '())  ;; no Node 'v in the given Graph
(define (edges-from-a-start-node graph s)
  (filter (lambda(x) (symbol=? (first x) s)) graph))

;; 2.
;; nodes-in-graph: [Set-of Edge] -> [List-of Symbol]
;; given a graph
;; return a list composed of all the edges' start Node and end Node,
;; each node may appear more than once because it can be a start Node for an edge
;; and it also can be an end Node for an edge
(check-expect (nodes-in-graph g1)
              (list 'e 'f 'f 'e 'e 'h 'f 'h 'h 'f 'f 'b 'a 'e 'a 'c
                    'h 'd 'b 'd 'c 'h 'g 'c 'd 'g 'g 'a 'g 'e 'i 'd 'e 'g 'i 'b 'i 'g 'c 'd))
(check-expect (nodes-in-graph g0) '())  ;; empty Graph
(define (nodes-in-graph graph)
  (foldr (lambda(x r) (list* (first x) (third x) r)) '() graph))

;; 3.
;; remove-duplicate-nodes: [List-of Symbol] -> [List-of Symbol]
;; given a list of Symbols
;; remove all the duplicate symbols and return a list of distinct Nodes
(check-expect (remove-duplicate-nodes
               (list 'e 'f 'f 'e 'e 'h 'f 'h 'h 'f 'f 'b 'a 'e 'a
                     'c 'h 'd 'b 'd 'c 'h 'g 'c 'd 'g 'g 'a 'g 'e 'i 'd))
              (list 'f 'b 'h 'c 'a 'g 'e 'i 'd))
(check-expect (remove-duplicate-nodes '()) '())  ;; empty list of Symbol
(check-expect (remove-duplicate-nodes '(f a f ag qg qg ag tq y sh th))
              (list 'a 'f 'qg 'ag 'tq 'y 'sh 'th))
(define (remove-duplicate-nodes alos)
  (foldr (lambda(x r) (if (member? x r)
                          r
                          (list* x r))) '() alos))

;; 4.
;; node-index: [List-of Symbol] Symbol -> NonNegInteger/-1
;; given a list of distinct Symbols,
;; return the given Symbol's index in the given list,
;; if the given Symbol does not exist in the given list, return -1
(check-expect (node-index (list 'f 'b 'h 'c 'a 'g 'e 'i 'd) 'h) 2)
(check-expect (node-index (list 'f 'b 'h 'c 'a 'g 'e 'i 'd) 'd) 8)
(check-expect (node-index (list 'f 'b 'h 'c 'a 'g 'e 'i 'd) 'w) -1)
(check-expect (node-index '() 'a) -1)
(define (node-index alos s)
  (if (member? s alos)
      (local [;; node-index-with-acc: [List-of Symbol] Symbol NonNegInteger -> NonNegInteger
              ;; given a list of distinct Symbols,
              ;; when the first Symbol in the list is same with the given Symbol,
              ;; return the index traversed so far
              ;; otherwise, traverse the rest of the list, add the acc
              ;; acc means the index for the list of Symbol we have traversed so far
              (define (node-index-with-acc alistos symbol acc)
                (cond [(symbol=? (first alistos) symbol) acc]
                      [else (node-index-with-acc (rest alistos) symbol (add1 acc))]))]
        (node-index-with-acc alos s 0))
      -1))

;; 5.
;; change-in-list: NonEmpty[List-of X] NonNegInteger X -> [List-of X]
;; given a non empty list of element X and the index you want to make a change in the list
;; return a list of element X with the specific index of element changed into the new one
;; the NonNegInteger idx should be in the range of [0, (length alox)) 
(check-expect (change-in-list (list 'a 10 "abc123" 20 'b 'work) 3 "good")
              (list 'a 10 "abc123" "good" 'b 'work))
(check-expect (change-in-list (list 'a 10 "abc123" 20 'b 'work) 0 "great")
              (list "great" 10 "abc123" 20 'b 'work))
(check-expect (change-in-list (list 'a 10 "abc123" 20 'b 'work) 5 15)
              (list 'a 10 "abc123" 20 'b 15))
(define (change-in-list alox idx x)
  (if (= 0 idx)
      (cons x (rest alox))
      (cons (first alox) (change-in-list (rest alox) (sub1 idx) x))))

;; 6.
;; minimum-path-without-except:
;; NonEmpty[List-of Number] NonEmpty[List-of Symbol] [List-of Symbol] -> Symbol/#false
;; given a non empty list of Number represent the distance
;; from the start Node to all the other nodes
;; and given a non empty list of Symbols represent the nodes
;; return the Node with the minimum distance from the start Node
;; which is not listed in the exception node list
;; if no nodes can be found, return #false
;; Length of alon and Length of alos should be same!
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'c 'a 'e 'ee 'cc))
              'd)
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff))
              #false)
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff 'sd))
              #false)  ;; more exceptions than need
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'a 'd 'e 'f))
              'dd)
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'a 'd 'e 'f 'dd))
              'c)
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ff))
              'ee)
(check-expect (minimum-path-without-except (list 12 50 23 15 373 52 124 +inf.0 123 21 35 +inf.0)
                                           (list 'a 'b 'c 'd 'e 'f 'aa 'bb 'cc 'dd 'ee 'ff)
                                           '())
              'a)
(define (minimum-path-without-except alon alos alosexcept)
  (if (<= (length alos) (length alosexcept))
      #false
      (local [(define validalos (foldr (lambda(x r) (if (member? x alosexcept)
                                                        r
                                                        (cons x r))) '() alos))
              ;; minimum-path-with-acc: NonNegInteger [List-of Symbol] -> Symbol
              ;; the NonNegInteger represents the current minimum path distance's Node index
              ;; in the given list of symbol alos
              ;; the second argument represents the list of symbols
              ;; that we need to compare with the
              ;; minimum path we got so far
              (define (minimum-path-with-acc minidx valos)
                (cond [(empty? valos) (list-ref alos minidx)]
                      [else (if (< (list-ref alon (node-index alos (first valos)))
                                   (list-ref alon minidx))
                                (minimum-path-with-acc
                                 (node-index alos (first valos)) (rest valos))
                                (minimum-path-with-acc minidx (rest valos)))]))]
        (minimum-path-with-acc (node-index alos (first validalos)) (rest validalos)))))



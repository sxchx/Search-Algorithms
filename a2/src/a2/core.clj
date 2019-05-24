(ns a2.core
	(require [clojure.string :as s])
	(require [clojure.java.io :as io])
  (:gen-class))


(def verbose false)

;==================================================================================================================

(defn read-map-from-file
  "takes a filename and reads in a map. The map will be in text format.
  A newly created state with a zero length path is returned by this function."
  [filename]
  ; read in map as a string
  (let [contents (slurp filename)
  		; remove the borders around the map
  		stripped (s/replace contents #"[-+|]" "")
		; split the string by lines
		split (s/split-lines stripped)]
		; create a state containing the map and an empty path
		{:map (subvec split 1), :path []})
 )

;==================================================================================================================

(defn find-port_
 "finds the coordinates of a port"
 ([mxp port]
 	; initial ver - calls the recurisve version
  (find-port_ mxp port 0))
  ; recursive ver - takes the remaining map, port letter and a counter for the y value
 ([rem-mxp port y]
 	; if we have read all values in the map then return nil
 	(if (empty? rem-mxp)
    	nil
		; if the first line of the remaining map contains the port
		(if (s/includes? (first rem-mxp) port)
			; then find the index of it (the x coord) and return as a vector the x and y coords
			[(s/index-of (first rem-mxp) port) y]
			; recursively call the function with the rest of the map.
			(recur (rest rem-mxp) port
				   (inc y)))))
)

(def find-port (memoize find-port_))

;==================================================================================================================

(defn goal
 "takes a state and which returns the coordinates of the destination
 port marked by G on the map"
 [state]
 ; calls the find port func passing it the map and the letter of the port to be found
 (find-port (get state :map) "G")
)

;==================================================================================================================

(defn start
 "takes a state and returns the coordinates of the starting port marked
 by S on the map"
 [state]
 ; calls the find port func passing it the map and the letter of the port to be found
 (find-port (get state :map) "S"))

;==================================================================================================================

(defn make-path_
 "applies the path to the map"
 [mxp x y path]
 ; get the line north and south of our currect pos, and our current pos too
 (let [north (get mxp (dec y))
 	   south (get mxp (inc y))
	   curr (get mxp y)]
		; if we have gone through the path return a vector containing the new map and curr x and y values
       (if (empty? path)
	  		[mxp x y]
			; if the first value of the remaining path is south
			(if (= (first path) :south)
				; recursively call the func passing it the map with a move south drawn in
				; and the new x and y coords of our ship and the rest of the path
				(recur (assoc mxp (inc y) (str (subs south 0 x) "." (subs south (inc x))))
					   x (inc y) (rest path))
				; repeat for north, east, and west
				(if (= (first path) :north)
					(recur (assoc mxp (dec y) (str (subs north 0 x) "." (subs north (inc x))))
						   x (dec y) (rest path))
					(if (= (first path) :east)
						(recur (assoc mxp y (str (subs curr 0 (inc x)) "." (subs curr (+ x 2))))
							   (inc x) y (rest path))
						(if (= (first path) :west)
							(recur (assoc mxp y (str (subs curr 0 (dec x)) "." (subs curr x)))
								   (dec x) y (rest path))
							; if the value in path is not north south east or west
							(println "Incorrect value in path")
								   ))))))
)

(def make-path (memoize make-path_))

;==================================================================================================================

(defn print-state
 "takes a state and pretty prints it to the console"
 [state]
 ;call find-s which takes a map and returns coords of S
 ;then call make-path to get to map with the path implemented
 (let [mxp (get state :map)
	   coords (find-port mxp "S")
	   x (first coords)
	   y (last coords)
	   path (get state :path)
     value (count (first mxp))
 	   new-map (first (make-path mxp x y path))]
     (println (str "+" (s/join (repeat value "-")) "+"))
		; loop through the map printing out each line
	   (loop [counter 0 to-print new-map]
	   	(when (< counter (count new-map))
			(println (str "|" (first to-print) "|"))
			(recur (inc counter) (rest to-print))))
     (println (str "+" (s/join (repeat value "-")) "+"))))

;==================================================================================================================

(defn position
 "takes a state and returns the coordinates of the ship on the map"
 [state]
 (let [mxp (get state :map)
 	   coords (find-port mxp "S")
	   x (first coords)
	   y (last coords)
	   path (get state :path)]
		; return a vector with the current x and y positions of the ship
	   [(get (make-path mxp x y path) 1) (get (make-path mxp x y path) 2)]))

;==================================================================================================================

(defn cost_
 "takes a state and returns the cost of the state"
 [state]
 ; count up the number of moves in the path
 (count (get state :path))
 )

 (def cost (memoize cost_))

 ;==================================================================================================================

(defn heuristic_
 "takes a state and computes its heuristic value using the Euclidean
distance metric"
 [state]
 ; get the x and y pos of the ship and the x and y pos of the goal state
 (let [px (first (position state))
	   py (last (position state))
	   gx (first (goal state))
	   gy (last (goal state))]
		; get the sqaure root of the sqaures of
		;(ship x pos minus goal x pos) plus (ship y pos minus goal y pos)
	   (Math/sqrt (+ (* (- px gx) (- px gx)) (* (- py gy) (- py gy))))
))

(def heuristic (memoize heuristic_))

;==================================================================================================================

(defn expand
 "takes a state returns a list of new states obtained by extending the
 length of the given state’s path by one in all possible valid directions."
 [state]
 		; get the x and y pos of the ship
 (let [x (first (position state))
 	   y (last (position state))
		; the map and path
	   mxp (get state :map)
	   path (get state :path)
		; the x and y boundaries of the map
	   x-out (count (first mxp))
	   y-out (count mxp)
		; the x and y pos of the start state
	   start-x (first (start state))
	   start-y (last (start state))]
		; loop through the possible moves south north east and west
		; and check if it doesnt exceed the boundaries, is not land nor
		; have we already passed this section.
	   (loop [new-states '() options [:south :north :east :west]]
			; if we have exhausted all moves return a list of possible states
	   	(if (empty? options)
			new-states
			; if the move to test is south
			(if (and (= (first options) :south) (= (get (get mxp (inc y)) x) \space)
					; and it is withing the boundaries of our map
					 (< (inc y) y-out) (< x x-out))
				; loop again with the new state added into the list and the rest of the moves
				(recur (conj new-states (assoc state :path (conj path :south)
													 :map (first (make-path mxp start-x start-y (conj path :south)))))
					   (rest options))
				; repeat for north east and west
				(if (and (= (first options) :north) (= (get (get mxp (dec y)) x) \space)
						 (< (dec y) y-out) (< x x-out))
					(recur (conj new-states (assoc state :path (conj path :north)
														 :map (first (make-path mxp start-x start-y (conj path :north)))))
						   (rest options))
					(if (and (= (first options) :east) (= (get (get mxp y) (inc x)) \space)
							 (< y y-out) (< (inc x) x-out))
						(recur (conj new-states (assoc state :path (conj path :east)
															:map (first (make-path mxp start-x start-y (conj path :east)))))
							   (rest options))
						(if (and (= (first options) :west) (= (get (get mxp y) (dec x)) \space)
								 (< y y-out) (< (dec x) x-out))
							(recur (conj new-states (assoc state :path (conj path :west)
																:map (first (make-path mxp start-x start-y (conj path :west)))))
								   (rest options))
							(recur new-states (rest options))
							))))))))


;==================================================================================================================


(defn best-first
	"takes a filename for a map and finds a path from start to goal using the best-first algorithm."
	;initial ver: takes filename, gets start state, creates frontier with start state, calls recursive version
	([filename]
		(let [state (read-map-from-file filename)]
		(best-first 0 [state]))
	)
	;recursive ver, takes an expansion counter and the frontier
	([expansions frontier]
   ; get the state with the min heuristic value from frontier and then remove it from frontier as we will expand it
		(let [min-h-val (apply min (map #(heuristic %) frontier))
				min-h-state (get frontier (.indexOf (map #(heuristic %) frontier) min-h-val))
				update-frontier (vec (remove #(= min-h-state %) frontier))
				;expand current state and for each neighbour check if that position already exists, keep the one with lowest h
				new-frontier (loop [n (expand min-h-state) states [] f update-frontier]
									(if (empty? n)
                    ;return the new frontier containing our kept neighbours
										(into [] (concat states f))
										(if (some #(= (position (first n)) (position %)) f)
											(if (< (heuristic (first n))
													 (heuristic (first (filter #(= (position (first n)) (position %)) f))))
												(recur (rest n) (conj states (first n))
														 (vec (concat (subvec f 0 (.indexOf (map #(position %) f) (position (first n))))
														 (subvec f (inc (.indexOf (map #(position %) f ) (position (first n))))))))
												(recur (rest n) states f))
											(recur (rest n) (conj states (first n)) f))))]
    ; if we have reached goal pos then print out solution
		(if (= min-h-val 1.0)
			(do
				(println "!!! SOLUTION !!!")
				(print-state min-h-state)
				(println "Number of state expansions:" expansions)
      )
			;otherwise recursively call function with our incremented expansions counter and the current frontier
			(do
        ;if verbose flag is set to true then print out the current state and its heuristic value
				(if (= verbose true)
					(do
						(print-state min-h-state)
						(recur (+ expansions 1) new-frontier))
					(recur (+ expansions 1) new-frontier)))
			)
		)
	)
)

;==================================================================================================================

(defn a-star
	"takes a filename for a map and finds a path from start to goal using the a-star algorithm"
	;initial ver: takes a filename, gets start state, creates frontier with start state, calls recursive version
	([filename]
		(let [state (read-map-from-file filename)]
		(a-star 0 [state]))
	)
	;recursive ver, takes an expansion counter and the frontier
	([expansions frontier]
   ;get the state with min heuristic + cost value from frontier then remove it from frontier as we will expand it
		(let [min-h-val (apply min (map #(+ (heuristic %) (cost %)) frontier))
				min-h-state (get frontier (.indexOf (map #(+ (heuristic %) (cost %)) frontier) min-h-val))
				update-frontier (vec (remove #(= min-h-state %) frontier))
				;expand current state and for each neighbour check if that position already exists, keep the one with lowest h+c
				new-frontier (loop [n (expand min-h-state) states [] f update-frontier]
									(if (empty? n)
                    ;return the new frontier containing our kept neighbours
										(into [] (concat states f))
										(if (some #(= (position (first n)) (position %)) f)
											(if (< (+ (heuristic (first n)) (cost (first n)))
													 (+ (heuristic (first (filter #(= (position (first n)) (position %)) f)))
													 	 (cost (first (filter #(= (position (first n)) (position %)) f)))))
												(recur (rest n) (conj states (first n))
														 (vec (concat (subvec f 0 (.indexOf (map #(position %) f) (position (first n))))
														 	(subvec f (inc (.indexOf (map #(position %) f) (position (first n))))))))
												(recur (rest n) states f))
											(recur (rest n) (conj states (first n)) f))))]
    ; if we have reached goal pos then print out solution
		(if (= (heuristic min-h-state) 1.0)
			(do
			   (println "!!! SOLUTION !!!")
			   (print-state min-h-state)
			   (println "Number of state expansions:" expansions)
      )
		   ;otherwise recursively call function with our incremented expansion counter and the current frontier
		   (do
        ;if verbose flag is set to true then print out the current state and its heuristic + cost value
				(if (= verbose true)
					(do
						(print-state min-h-state)
						(recur (+ expansions 1) new-frontier))
					(recur (+ expansions 1) new-frontier)))
			)
		)
	)
)

;==================================================================================================================


(defn -main
  "Print out all four maps using a-star and the same for best-first"
  [& args]
  (println "A-Star")
  (println "Map 1:")
  (a-star "map.txt")
  (println "Map 2:")
  (a-star "map2.txt")
  (println "Map 3:")
  (a-star "map3.txt")
  (println "Map 4:")
  (a-star "map4.txt")
  (println "Best-First")
  (println "Map 1:")
  (best-first "map.txt")
  (println "Map 2:")
  (best-first "map2.txt")
  (println "Map 3:")
  (best-first "map3.txt")
  (println "Map 4:")
  (best-first "map4.txt")
)


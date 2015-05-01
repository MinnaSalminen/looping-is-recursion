(ns looping-is-recursion)


; EXERCISE 1
(defn power [base exp]
  (let [acc 1
        n exp
        helper (fn [acc n]
                 (cond (zero? n) 1
                       (= 1 n) acc
                       (neg? n) (/ (power base (- n)))
                       :else (recur (* acc base) (dec n))))]
    (helper base exp)))

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0


; EXERCISE 2
(defn last-element [a-seq]
  (let [helper (fn [a]
                 (cond (empty? (rest a)) (first a)
                       :else   (recur (rest a))))
        ]
    (helper a-seq)))

;(last-element [])      ;=> nil
;(last-element [1 2 3]) ;=> 3
;(last-element [2 5])   ;=> 5


; EXERCISE 3
(defn seq= [seq1 seq2]
  (let[helper (fn [a b]
                (let [x (first a)
                      y (first b)
                      xr (rest a)
                      yr (rest b)]
                  (cond (and (empty? a) (empty? b)) true
                        (or (empty? a) (empty? b)) false
                        (not (= a b)) false
                        :else (recur xr yr))
                  ))]
    (helper seq1 seq2)) )

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false


; EXERCISE 4
(defn find-first-index [pred a-seq]
  (loop [n 0
         r a-seq]
    (cond (empty? r) nil
          (pred (first r)) n
          :else (recur (inc n) (rest r)))
    ))

;(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
;(find-first-index zero? [1 1 3 7 2])                          ;=> nil
;(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
;(find-first-index nil? [])                                    ;=> nil


; EXERCISE 5
(defn avg [a-seq]
  (loop [n 0
         s 0
         r a-seq]
    (cond (empty? r) (if (zero? n)
                       0
                       (/ s n)
                       )
          :else (recur (inc n) (+ s (first r)) (rest r)))
    ))

;(avg [1 2 3])   ;=> 2
;(avg [0 0 0 4]) ;=> 1
;(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5


; EXERCISE 6
(defn toggle [a-set elem]
  "Parityn apufunktiona"
  (if (contains? a-set elem )
    (disj a-set elem )
    (conj a-set elem )
    ) )

(defn parity [a-seq]
  (loop [acc #{}
         r a-seq]
    (cond (empty? r) acc
          :else (recur (toggle acc (first r)) (rest r)) )))


; EXERCISE 7
(defn fast-fibo [n]
  (loop [i 0
         a -1
         b 1]
    (let [c (+ a b)]
      (cond (>= i n) c
            :else (recur (inc i) b c)))))

;(fast-fibo 0) ;=> 0
;(fast-fibo 1) ;=> 1
;(fast-fibo 2) ;=> 1
;(fast-fibo 3) ;=> 2
;(fast-fibo 4) ;=> 3
;(fast-fibo 5) ;=> 5
;(fast-fibo 6) ;=> 8


; EXERCISE 8
(defn cut-at-repetition [a-seq]
  (loop [coll #{}
         acc []
         r a-seq]
    (cond (empty? r) acc
          (contains? coll (first r)) acc
          :else (recur (conj coll (first r)) (conj acc (first r)) (rest r)) )))

;(cut-at-repetition [1 1 1 1 1]) ;=> [1] doesn't have to be a vector, a sequence is fine too
;(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog]) ;=> [:cat :dog :house :milk 1]
;(cut-at-repetition [0 1 2 3 4 5]) ;=> [0 1 2 3 4 5]


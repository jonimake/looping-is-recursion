(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1, n exp]
    (if (= n 0)
      acc
      (recur (* acc base) (- n 1)))))

(defn last-element [a-seq]
  (let [helper (fn [s]
                 (if (empty? (rest s))
                   (first s)
                   (recur (rest s))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [xs ys]
                 (cond
                  (and (empty? xs) (empty? ys)) true
                  (empty? xs) false
                  (empty? ys) false
                  (= (first xs) (first ys)) (recur (rest xs) (rest ys))
                  :else false))]
  (helper seq1 seq2)))



(defn find-first-index [pred a-seq]
  (loop [i 0, xs a-seq]
    (cond
      (empty? xs) nil
      (pred (first xs)) i
      :else (recur (+ i 1) (rest xs)))))

(defn avg [a-seq]
  (loop [sum 0, count 0, xs a-seq]
    (if (empty? xs)
      (/ sum count)
      (recur (+ sum (first xs)) (+ count 1) (rest xs)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{} xs a-seq]
    (if (empty? xs)
      acc
      (recur (toggle acc (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (loop [prev 0, cur 1, n n]
    (if (<= n 0)
      prev
      (recur cur (+ prev cur) (- n 1)))))

(defn cut-at-repetition [a-seq]
  (loop [acc [], xs a-seq]
    (cond
     (empty? xs) acc
     (= (some #{(first xs)} acc) (first xs)) acc
     :else (recur (conj acc (first xs)) (rest xs)))))


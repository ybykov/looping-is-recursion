(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res base exp]
                 (if (zero? exp)
                   res
                   (recur (* res base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [ls a-seq]
                 (if (empty? a-seq)
                   ls
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                  (not= (count seq1) (count seq2)) false
                  (empty? seq1) acc
                  :else (recur (= (first seq1) (first seq2)) (rest seq1) (rest seq2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         cur a-seq]
    (cond
     (empty? cur) nil
     (pred (first cur)) index
     :else (recur (inc index) (rest cur)))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         cur a-seq]
    (if (empty? cur)
      (/ sum cnt)
      (recur (+ sum (first cur)) (inc cnt) (rest cur)))))

(defn parity [a-seq]
  (loop [acc ()
         seq (sort a-seq)]
    (if (empty? seq)
      acc
      (recur
       (if (.contains (rest seq) (first seq))
         acc
         (cons (first seq) acc))
       (if (.contains (rest seq) (first seq))
         (rest (rest seq))
         (rest seq))))))


(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (loop [fnn 0
                fn  1
                cur 2]
           (cond
            (= cur n) (+ fnn fn)
            :else  (recur fn (+ fn fnn) (inc cur))))))

(defn cut-at-repetition [a-seq]
  (loop [el (first a-seq)
         acc [(first a-seq)]
         tail (rest a-seq)]
    (cond
     (empty? tail) acc
     (= (first tail) el) acc
     :else (recur el (conj acc (first tail)) (rest tail)))))




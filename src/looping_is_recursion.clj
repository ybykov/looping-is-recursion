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
  (let [helper (fn [seq1 seq2]
                 (cond
                  (not= (count seq1) (count seq2)) false
                  (empty? seq1) true
                  :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

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
  )

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])


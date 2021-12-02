(ns tggreene.tidbits.core)

(defn update-with
  "Returns a map with translations and functions specified by mf applied. A scalar
  key will apply it's value to the matching key in a given map. A collection key
  of two elements will map the key matching it's first element in a given map to
  it's second element and then apply it's value. A collection of two elements
  where the first element is a collection will take all specified scalar keys
  from a given map and pass them as arguments to the function specified by the
  value giving the second element as the resulting key. It's probably much
  clearer with examples:

  (update-with
   {:a inc}
   {:a 1
    :b 2})
  ;; => {:a 2 :b 2}

  (update-with
   {[:a :c] inc}
   {:a 1
    :b 2})
  ;; => {:b 2 :c 2}

  (update-with
   {[[:a :b] :c] +}
   {:a 1
    :b 2})
  ;; => {:c 3}

  (update-with
   {[:a :b] identity
    :c inc
    [nil :d] (constantly 1)
    [[:e :f] :g]
    (fn [e f]
      (/ e f))}
   {:a 3
    :c 5
    :e 4
    :f 2}
  ;; => {:b 3
         :c 6
         :d 1
         :g 2}"

  [mf m]
  (reduce
   (fn [m [k v]]
     (cond
       (and (sequential? k)
            (not (sequential? (first k))))
       (-> m
           (dissoc (first k))
           (assoc (second k) (v (get m (first k)))))

       (and (sequential? k)
            (sequential? (first k)))
       (-> (apply dissoc m (first k))
           (assoc (second k) (apply v (vals (select-keys m (first k))))))

       :else (update m k v)))
   m
   mf))

(defn dedupe-by
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (let [pv (volatile! false)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv
                current (pred input)]
            (vreset! pv current)
            (if (and prior current)
              result
              (rf result input))))))))
  ([pred coll] (sequence (dedupe-by pred) coll)))

(defn split-whenever
  "Lazily split whenever a function f returns a different value on an element of
  collection coll"
  [f coll]
  (lazy-seq
   (when (seq coll)
     (let [[h & t] coll
           [xs ys] (split-with #(= (f h) (f %)) t)]
       (cons (cons h xs)
             (split-whenever f ys))))))

(defn safe-div
  "Returns nil if values aren't expected types or b is not a pos int"
  [a b]
  (when (and (number? a) (number? b) (not= 0 b))
    (/ a b)))

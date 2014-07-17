(ns rooten.jquery
  "From jayq (https://github.com/ibdknox/jayq)
   Using the library itself wasn't working for me... will give it another go when I have time")

(defn crate-meta [func]
  (.-prototype._crateGroup func))

(defn ->selector [sel]
  (cond
   (string? sel) sel
   (fn? sel) (if-let [cm (crate-meta sel)]
               (str "[crateGroup=" cm "]")
               sel)
   (keyword? sel) (name sel)
   :else sel))

(defn $
    ([sel]
     (js/jQuery (->selector sel)))
  ([sel context]
     (js/jQuery (->selector sel) context)))

(defn add-class [$elem cl]
  (.addClass $elem (name cl)))

(defn remove-class
  ([$elem]
   (.removeClass $elem))
  ([$elem cl]
   (.removeClass $elem (name cl))))

(defn trigger [$elem ev]
  (.trigger $elem (name ev)))

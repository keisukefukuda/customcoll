(ns customcoll.map)

(defn is-fn? [sym]
  "Check if sym is a function definition.
   Used in the macros"
  (cond
    (symbol? sym)
    (-> sym resolve var-get fn?)
    
    (instance? clojure.lang.ISeq sym)
    true
    
    :else false))

(defn assert-fn [sym]
  (assert (is-fn? sym)
          (str "A function is expected but given " sym)))

;; hooks:
;; not-found
;; empty
;; APersistentMapは、java.lang.Mapとして比較
;; java.util.Mapをimplement  Map$Entry
;; MapEquivalence
(defn custom-map
  ([map-data] (custom-map map-data {}))
  ([map-data opts]
   (let [{vh :value-hook} opts]
     (reify
       clojure.lang.Associative
       clojure.lang.IPersistentMap
       (entryAt [this k]
         (if (contains? map-data k)
           (let [v (map-data k)
                 v (if vh (vh v) v)]
             (clojure.lang.MapEntry. k v))
           nil))
     
       (assoc [this key val]
         (custom-map (.assoc map-data key val) opts))

       (assocEx [this key val] (throw (Exception. "error")))
       
       (without [this key]
         (custom-map (.without map-data opts)))
       
       clojure.lang.IFn
       (invoke [this key]
         (.valAt this key))
       
       clojure.lang.ILookup
       (valAt [this key]
         (.valAt this key nil))
       
       (valAt [this key not-found]
         (if (contains? map-data key)
           (let [v (.valAt map-data key)]
             (if vh (vh v) v))
           not-found))
       
       clojure.lang.IPersistentCollection
       (count [_] (.count map-data))
       (empty [_] (.empty map-data))
       (equiv [this o]
         (.equiv o this))
       (cons [_ s]
         (custom-map (.cons map-data s) opts))

       (containsKey [_ key]
         (.containsKey map-data key))

       java.util.Map
       (clear [_] (throw (UnsupportedOperationException.)))
       (entrySet [this]
         (reify java.util.Set
           (iterator [_] (.iterator this))
           (size [_] (.count this))
           (contains [_ o]
             (if (instance? java.util.Map$Entry o)
               (if-let [v (map-data (.getKey o))]
                 (if (= v (.getValue o))
                   true
                   false)
                 false)
               false))))
       (isEmpty  [this] (zero? (.count this)))
       (size [this] (count map-data))
       (get [this key] (.get map-data key))
       (put [this key val] (throw (UnsupportedOperationException.)))

       java.lang.Iterable
       (iterator [this]
         (let [s (atom (seq this))]
           (reify java.util.Iterator
             (hasNext [this] (not (nil? @s)))
             (next    [this] (let [v (first @s)]
                               (swap! s next)
                               v))
             (remove  [this] (throw (UnsupportedOperationException.))))))

       clojure.lang.Seqable
       (seq [_]
         (map #(let [[k v] %
                     v (if vh (vh v) v)]
                 (clojure.lang.MapEntry. k v))
              map-data))
       
       clojure.lang.Reversible
       (rseq [_] (reverse (seq _)))

       clojure.lang.MapEquivalence))))


(defn zipmapf [ks f] (zipmap ks (map f)))

;; reifyを使った形式が、筋が良いかも
;;
;; (custom-map (zipmap keys (map f keys))
;;
;; (custom-map (zipmapf keys f) {...})
;;
;; (custom-map {}
;;             {:val-filter vf}
;;             (toString [_] )
;; 
;; Mapのequivについて調べる必要がある？equivalenceはどう実装されるのか？
;;   * hash-mapとarray-mapは、=で比較すれば等価だろうが、.equivで比較するとどうか？
;;   * APersistentMap::equiv() で用いられているのはどのような実装か？
;;   * clojure.core/= では具体的にどのように比較が実装されるか？
;;
;; .cons/assoc 等はどう実装されるべきか？
;; 目的を考えると･･･どうなるか。とりあえずpaperknifeを基準に考えると、望ましい動作はどうなるか？
;; (def ^:dynamic *print-ref*)
;; key-filter, val-filterを考えると、JoinMap的なものを返すべきか
;;
;; さらに、toString的なやつを実装するためにはどうすればよいのか？それを => toStringメソッド
;; この機能として実装することができるか？
;;

(deftype MyMap1 [ks f]
  clojure.lang.Associative
  clojure.lang.IPersistentMap
  (entryAt [this k]
    (if (some #{k} ks)
      (reify clojure.lang.IMapEntry
        (key [this] k)
        (val [this] (f k)))
      nil))

  (assoc [this key val]
    this)

  (assocEx [this key val] (throw (Exception. "error")))

  (without [this key]
    this)

  clojure.lang.IFn
  (invoke [this k]
    (let [v (.valAt this k)]
      v))

  clojure.lang.ILookup
  (valAt [this k]
    (if (some #{k} ks)
      (f k)
      nil))

  (valAt [this k not-found]
    (if (some #{k} ks)
      (.valAt this k)
      not-found))

  clojure.lang.IPersistentCollection
  (count [_] (.count ks))
  (empty [_] (.empty ks))
  (equiv [this o]
    (and (instance? o MyMap1)
         (= (keys o) ks)
         (= (vals o) (map f keys))))

  (cons [_ v] _)

  java.lang.Iterable
  (iterator [this]
    (let [s (seq this)]
      (reify java.util.Iterator
        (hasNext [this] (.hasNext s))
        (next    [this] (.next s))
        (remove  [this] (.remove s)))))

  clojure.lang.Seqable
  (seq [_] (map #(reify java.util.Map$Entry
                   (getKey [_] %)
                   (getValue [_] (f %)))
                ks))

  clojure.lang.Reversible
  (rseq [_] (reverse (seq _))))


(deftype MyMap [content]
  clojure.lang.Associative
  clojure.lang.IPersistentMap

  (entryAt [_ key]
    (.entryAt content key))
  (assoc [_ key val]
    (MyMap. (.assoc content key val)))

  (assocEx [_ val val]
    (throw (Exception. "error")))

  (without [_ val]
    (MyMap. (.without content val)))

  clojure.lang.ILookup
  (valAt [_ key]
    (.valAt content key))
  (valAt [_ key not-found]
    (.valAt content not-found))

  clojure.lang.IPersistentCollection
  (count [_]
    (.count content))
  (empty [_]
    (.empty content))
  (equiv [_ o]
    (and (instance? o MyMap)
         (.equiv content o)))
  (cons [_ v]
    (MyMap. (.cons content v)))

  java.lang.Iterable
  (iterator [this]
    (let [s (seq this)]
      (reify java.util.Iterator
        (hasNext [this] (.hasNext s))
        (next    [this] (.next s))
        (remove  [this] (.remove s)))))

  clojure.lang.Seqable
  (seq [_] (seq '((1 2) (3 4))))

  clojure.lang.Reversible
  (rseq [_] (reverse (seq _)))
  )

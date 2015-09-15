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

(defn call-with-opts [f args kwd]
  (apply f (concat args (flatten (vec kwd)))))

;; hooks:
;; not-found
;; empty
;; APersistentMapは、java.lang.Mapとして比較
;; java.util.Mapをimplement  Map$Entry
;; MapEquivalence
(defn custom-map
  ([map-data & {vh :value-hook :as opts}]
   (letfn [(->normal-map [this] (apply hash-map (flatten (vec this))))]
     (reify
       clojure.lang.Associative
       clojure.lang.IPersistentMap
       (entryAt [this k]
         (if (contains? map-data k)
           (let [v (.valAt this k)]
             (clojure.lang.MapEntry. k v))
           nil))
       
       (assoc [this key val]
         (merge (->normal-map this) {key val}))

       (assocEx [this key val] (throw (Exception. "error")))
       
       (without [this key]
         (call-with-opts custom-map [(.without map-data key)] opts))
       
       clojure.lang.IFn
       (invoke [this key]
         (.valAt this key))
       
       clojure.lang.ILookup
       (valAt [this key]
         (try
           (.valAt this key nil)
           (catch Exception e
             (let [msg (.getMessage e)]
               (throw (Exception. (str "custom-map: :value-hook hander threw an exception with key "
                                       (pr-str key)
                                       ". Original exception: "
                                       (-> e type pr-str)
                                       " : "
                                       msg)))))))
       
       
       (valAt [this key not-found]
         (if (contains? map-data key)
           (let [v (.valAt map-data key)]
             (if vh (vh v) v))
           not-found))
       
       clojure.lang.IPersistentCollection
       (count [_] (.count map-data))
       (empty [_] (.empty map-data))
       (equiv [this o]
         (and (= (.size this) (.size o))
              (= (keys this) (keys this))
              (loop [ks (keys this)]
                (if (nil? ks)
                  true
                  (let [k (first ks)
                        v (.valAt this k)
                        ov (.valAt o k)]
                    (if (= v ov)
                      (recur (next ks))
                      false))))))
       (cons [this s]
         (.cons (->normal-map this) s))
       
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
               (let [k (.getKey o)]
                 (if (contains? map-data k)
                   (let [v (.valAt this k)]
                     (= v (.getValue o)))
                   false)
                 false)
               false))))
       (isEmpty  [this] (zero? (.count this)))
       (size [this] (count map-data))
       (get [this key] (.valAt this key))
       (put [this key val] (throw (UnsupportedOperationException.)))

       java.lang.Iterable
       (iterator [this]
         (let [s (atom (seq this))]
           (reify java.util.Iterator
             (hasNext [this] (not (nil? @s)))
             (next    [this] (let [ent (first @s)]
                               (swap! s next)
                               ent))
             (remove  [this] (throw (UnsupportedOperationException.))))))

       clojure.lang.Seqable
       (seq [this]
         (map #(let [[k _] %
                     v (.valAt this k)]
                 (clojure.lang.MapEntry. k v))
              map-data))
       
       clojure.lang.Reversible
       (rseq [_] (reverse (seq _)))

       clojure.lang.MapEquivalence))))


(defn zipmapf [ks f] (zipmap ks (map f)))

;; (custom-map (zipmap keys (map f keys))
;;
;; (custom-map (zipmapf keys f) {...})
;;
;; (custom-map {}
;;             {:val-filter vf}
;;             (toString [_] )

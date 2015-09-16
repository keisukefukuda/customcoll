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
  ([map-data & {value-hook :value-hook
                front-map :__front-map
                :as opts}]
   (letfn [(->normal-map [this] (apply hash-map (flatten (vec this))))]
     (let [map-data (apply dissoc map-data (keys front-map))]
       (reify
         clojure.lang.Associative
         clojure.lang.IPersistentMap
         (entryAt [this k]
           (if (contains? map-data k)
             (let [v (.valAt this k)]
               (clojure.lang.MapEntry. k v))
             nil))
         
         (assoc [this key val]
           (custom-map map-data
                       :value-hook value-hook
                       :__front-map (assoc front-map key val)))
         
         (assocEx [this key val] (throw (Exception. "error")))
         
         (without [this key]
           (custom-map (dissoc map-data key)
                       :value-hook value-hook
                       :__fron-map (dissoc front-map key)))
         
         clojure.lang.IFn
         (invoke [this key]
           (.valAt this key))

         clojure.lang.ILookup
         (valAt [this key]
           (.valAt this key nil))
         
         (valAt [this key not-found]
           (if (and front-map (front-map key))
             (front-map key)
             (if (contains? map-data key)
               (let [v (.valAt map-data key)]
                 (try
                   (if value-hook
                     (value-hook v)
                     v)
                   (catch Exception e
                     (let [msg (.getMessage e)]
                       (throw (Exception. (str "custom-map: :value-hook hander threw an exception with key "
                                               (pr-str key)
                                               ". Original exception: "
                                               (-> e type pr-str)
                                               " : "
                                               msg)))))))
               not-found)))
         
         clojure.lang.IPersistentCollection
         (count [_] (+ (.count (or map-data {}))
                       (.count (or front-map {}))))
         (empty [_] (and (.empty map-data) (.empty front-map)))
         (equiv [this o]
           (and (= (.size this) (.size o))
                (loop [ks (keys this)]
                  (if (or (nil? ks) (empty? ks))
                    true
                    (let [k (first ks)
                          v (.valAt this k)
                          ov (.valAt o k)]
                      (if (= v ov)
                        (recur (next ks))
                        false))))))
         (cons [this coll]
           (cond
             (instance? java.util.Map$Entry coll)
             (let [k (.getKey coll)
                   v (.getValue coll)]
               (.assoc this key val))

             (instance? clojure.lang.IPersistentVector coll)
             (do 
               (when-not (= 2 (count coll) )
                 (throw (java.lang.IllegalArgumentException. "Vector arg to map conj must be a pair")))
               (let [key (nth coll 0)
                     val (nth coll 1)]
                 (.assoc this key val)))

             :else
             (loop [s (seq coll) ret this]
               (if (nil? s)
                 ret
                 (let [^java.util.Map$Entry ent (first s)
                       key (.getKey ent)
                       val (.getValue ent)]
                   (recur (next s)
                          (.assoc ret key val)))))))
             
         
         (containsKey [_ key]
           (or (.containsKey (or front-map {}) key)
               (.containsKey map-data key)))
         
         java.util.Map
         (clear [_] (throw (UnsupportedOperationException.)))
         (entrySet [this]
           (reify java.util.Set
             (iterator [_] (.iterator this))
             (size [_] (.count this))
             (contains [_ o]
               (if (instance? java.util.Map$Entry o)
                 (let [k (.getKey o)]
                   (if (.containsKey this k)
                     (let [v (.valAt this k)]
                       (= v (.getValue o)))
                     false)
                   false)
                 false))))
         (isEmpty  [this] (zero? (.count this)))
         (size [this] (.count this))
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
           (let [ks (concat (keys front-map) (keys map-data))]
             (map #(let [k %
                         v (.valAt this k)]
                     (clojure.lang.MapEntry. k v))
                  ks)))
         
         clojure.lang.Reversible
         (rseq [_] (reverse (seq _)))

         clojure.lang.MapEquivalence)))))
  

(defn zipmapf [ks f] (zipmap ks (map f)))

;; (custom-map (zipmap keys (map f keys))
;;
;; (custom-map (zipmapf keys f) {...})
;;
;; (custom-map {}
;;             {:val-filter vf}
;;             (toString [_] )

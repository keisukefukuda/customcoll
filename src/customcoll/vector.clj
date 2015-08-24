(ns customcoll.vector)

(defn- default-vector-funcs [type-name args]
  "Returns the default method definitions for vector-compatibe types."
  (let [ctor (symbol (str "." type-name))
        base-vec (first args)
        base-vec-kw (keyword base-vec)]
    ;; base-vec is a symbol of the first field of the deftype (such as 'nth)
    ;; and the following is how methods are expressed in our defcustomvec macro.
    { ['nth 2]     `(~'nth [_# i#] (.nth ~base-vec i#))
      ['nth 3]     `(~'nth [_# i# nf#] (.nth ~base-vec i# nf#))
      ['valAt 2]   `(~'valAt [_# i#] (.valAt ~base-vec i#))
      ['valAt 3]   `(~'valAt [_# i# nf#] (.valAt ~base-vec i# nf#))
      ['entryAt 2] `(~'entryAt [_# i#] (.entryAt ~base-vec i#))
      ['assoc 3]   `(~'assoc [_# key# val#] (~ctor (.assoc ~base-vec key# val#) ~@(rest args)))
      ['peek 1]    `(~'peek [_#] (.peek ~base-vec))
      ['pop 1]     `(~'pop [_#] (~ctor (.pop ~base-vec) ~@(rest args)))
      ['count 1]   `(~'count [_#] (.count ~base-vec))
      ['empty 1]   `(~'empty [_#] (.empty ~base-vec))
      ['equiv 2]   `(~'equiv [_# o#] (and (isa? (class o#) ~type-name)
                                          (.equiv (~base-vec-kw o#) ~base-vec)))
      ['assocN 3]  `(~'assocN [_# i# val#] (~ctor (.assocN ~base-vec i# val#) ~@(rest args)))
      ['cons 2]    `(~'cons [_# o#] (~ctor (.cons ~base-vec o#) ~@(rest args)))
      ['seq 1]     `(~'seq [this#]
                           (letfn [(seq'# [idx#] (if (>= idx# (count ~base-vec))
                                                   nil
                                                   (cons (.nth this# idx#)
                                                         (lazy-seq (seq'# (inc idx#))))))]
                             (seq'# 0)))
      ['rseq 1]    `(~'rseq [this#]
                            (letfn [(rseq'# [idx#] (if (< idx# 0)
                                                     nil
                                                     (cons (.nth this# idx#)
                                                           (lazy-seq (rseq'# (dec idx#))))))]
                              (rseq'# (dec (count ~base-vec)))))
      }))

(defn- compl-vector-access2-funcs [funcs]
  ;; In vector implementation, entryAt, nth(2), valAt(2) are identical
  ;; so we need only one of them.
  ;; Thus, if any of nth,valAt,entryAt is defined, then we can define
  ;; the other 2 functions using it.
  (let [f (cond (funcs ['nth 2]) 'nth
                (funcs ['valAt 2]) 'valAt
                (funcs ['entryAt 2]) 'entryAt
                :else nil)
        f (symbol (str "." f))]
    (if (nil? f)
      funcs ;; the default definitions are used for all of (nth, valAt, entryAt)
      (loop [gs ['nth 'valAt 'entryAt]
             funcs funcs]
        (if (empty? gs)
          funcs
          (let [g (first gs)]
            (if (nil? (funcs [g 2]))
              (recur (rest gs) (assoc funcs [g 2] `(~g [this# i#] (~f this# i#))))
              (recur (rest gs) funcs))))))))

(defn- compl-vector-access3-funcs [funcs]
  ;; In vector implementation, nth(3), valAt(3) are identical
  ;; so we need only one of them.
  (let [f (cond (funcs ['nth 3]) 'nth
                (funcs ['valAt 3]) 'valAt
                :else nil)]
    (if (nil? f)
      funcs ;; the default definitions are used for all of (nth, valAt, entryAt)
      (let [f (symbol (str "." f))]
        (loop [gs ['nth 'valAt]
               funcs funcs]
          (if (empty? gs)
            funcs
            (let [g (first gs)]
              (if (nil? (funcs [g 3]))
                (recur (rest gs) (assoc funcs [g 3] `(~g [this# i# not-found#] (~f this# i# not-found#))))
                (recur (rest gs) funcs)))))))))
  
(defn- build-vector-def [type-name args funcs]
  (let [base-vec (first args)]
    `(deftype ~type-name ~args
       clojure.lang.Associative
       ~(funcs ['entryAt 2])
       ~(funcs ['assoc 3])
       
       clojure.lang.IPersistentStack
       ~(funcs ['peek 1])
       ~(funcs ['pop 1])

       clojure.lang.Indexed
       ~(funcs ['nth 2])
       ~(funcs ['nth 3])

       clojure.lang.IPersistentCollection
       ~(funcs ['count 1])
       ~(funcs ['empty 1])
       ~(funcs ['equiv 2])

       clojure.lang.ILookup
       ~(funcs ['valAt 2])
       ~(funcs ['valAt 3])

       clojure.lang.IPersistentVector
       ~(funcs ['assocN 3])
       ~(funcs ['cons 2])

       clojure.lang.Sequential

       clojure.lang.Seqable
       ~(funcs ['seq 1])

       clojure.lang.Reversible
       ~(funcs ['rseq 1])
       )))

;; nth count peek pop equiv
(defmacro defcustomvec [type-name args & specs]
  ;; Check args is a vector
  (if-not (vector? args)
    (throw (Exception. "The first argument of defcustomvec must be a vector of deftype members")))

  (when (empty? args)
    (throw (Exception. "defcustomvec: at least one field must be specified as a base vector")))

  (let [base-vec (first args)]
    ;; if one of args has ^derives attribute, use it as a "base class" of the custom vector.
    (loop [specs specs funcs {}]
      (if (empty? specs)
        ;; done
        (build-vector-def type-name args (->> funcs
                                              compl-vector-access2-funcs
                                              compl-vector-access3-funcs
                                              (merge (default-vector-funcs type-name args))
                                              ))
          
        ;; process next function
        (let [func (first specs)
              fname (first func)
              fargs (second func)]
          (recur (rest specs)
                 (assoc funcs [fname (count fargs)] func)))))))


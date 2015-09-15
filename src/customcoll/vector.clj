(ns customcoll.vector)

(defn- default-vector-funcs [type-name args]
  "Returns the default method definitions for vector-compatibe types."
  (let [ctor (symbol (str type-name "."))
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

(defn- fm-func-body [funcmap func-name nargs]
  (nth (funcmap [func-name nargs]) 2))

(defn- fm-func-args [funcmap func-name nargs]
  (nth (funcmap [func-name nargs]) 1))

(defn- fm-copy-shared-bodies
  "funcmap is a map like defined in default-vector-funcmap
   In custom vector definition, some functions have same meaning and
   the bodies can be shared.
   Let the 'shared-body' functions are A, B, and C, and A is actually defined,
   copy-shared-bodies copy the body of A to B and C.
   func-list is expected to be a 2-element vector of function name symbol and
   number of arguments."
  [funcmap nargs & func-syms]
  (let [orig-func-sym  (first (filter #(funcmap [% nargs]) func-syms))
        rest-func-syms (remove #(= % orig-func-sym) func-syms)
        fargs (fm-func-args funcmap orig-func-sym nargs)
        fbody (fm-func-body funcmap orig-func-sym nargs)]
    (if (nil? orig-func-sym)
      funcmap
      (loop [rest-func-syms rest-func-syms funcmap funcmap]
        (let [f (first rest-func-syms)]
          (cond
            (empty? rest-func-syms) funcmap
            (funcmap [f nargs]) (recur (rest rest-func-syms) funcmap) 
            :else (recur (rest rest-func-syms)
                         (assoc funcmap [f nargs] (list f fargs fbody)))))))))

(defn- compl-vector-access2-funcs [funcs]
  (fm-copy-shared-bodies funcs 2 'nth 'valAt 'entryAt))

(defn- compl-vector-access3-funcs [funcs]
  (fm-copy-shared-bodies funcs 3 'nth 'valAt))

(defn- compl-vector-assoc-funcs [funcs]
  (fm-copy-shared-bodies funcs 3 'assoc 'assocN))

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
                                              compl-vector-assoc-funcs
                                              (merge (default-vector-funcs type-name args))
                                              ))
          
        ;; process next function
        (let [func (first specs)
              fname (first func)
              fargs (second func)]
          (recur (rest specs)
                 (assoc funcs [fname (count fargs)] func)))))))


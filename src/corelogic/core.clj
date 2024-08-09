(ns corelogic.core
  {:clj-kondo/config
   '{:linters {:unresolved-symbol {:exclude-patterns ["\\?\\w"]}}}}
  [:require 
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]])

#_(defn nato [n]
   (l/conde
     [(l/== n 'z)]
     [(l/fresh [?n-1]
               (l/== n `(~'s ~?n-1))
               (nato ?n-1))]))

#_(l/run 6 [?q] (nato ?q))

#_(defn succo [n s]
   (nato n)
   (nato s) ;only defined on the naturals
   (l/== s `(~'s ~n)))

#_(l/run 1 [?q]
       (succo '(s (s (s (s (s z))))) ?q))

#_(defn pluso [n1 n2 n3]
   (l/conde
     [(l/== n1 'z)
      (l/== n3 n2)]
     [(l/fresh [?n1p ?n3p]
             (l/== n1 `(~'s ~?n1p))
             (l/== n3 `(~'s ~?n3p))
             (pluso ?n1p n2 ?n3p))]))


#_(l/run 3 [?q] 
        (l/fresh [?n1 ?n2 ?n3 ?n4]
                 (l/== ?q [?n1 ?n2 ?n3])
                 (pluso ?n2 ?n3 ?n4)
                 (pluso ?n1 ?n4 '(s (s (s (s (s z))))))))

#_(defn multo [n1 n2 n3]
   (l/conde
     [(l/== n1 'z)
      (l/== n3 'z)]
     [(l/fresh [?left ?right ?n1*n2]
               ())]))


;Find all valid combinations of k numbers that sum up to n such that the following conditions are true:
;
;    Only numbers 1 through 9 are used.
;    Each number is used at most once.
;
;Return a list of all possible valid combinations. The list must not contain the same combination twice, and the combinations may be returned in any order.
#_(l/run* [?q]
          (l/fresh [?v0 ?v1 ?v2]
                   ;In the domain
                   (fd/in ?v0 (fd/domain 1 2 3 4 5 6 7 8 9))
                   (fd/in ?v1 (fd/domain 1 2 3 4 5 6 7 8 9))
                   (fd/in ?v2 (fd/domain 1 2 3 4 5 6 7 8 9))
                   ;Each number is unique
                   (fd/distinct [?v0 ?v1 ?v2])
                   ;The numbers sum to n
                   (fd/eq
                     (= 9 (+ ?v0 ?v1 ?v2)))
                   ;Output
                   (l/== ?q [?v0 ?v1 ?v2])))
#_(defmacro makerel-lists [k n lvar]
    (let [vars (map #(symbol (str "?v" %)) (range k))
          varsvec (vec vars)
          domcons (->> vars
                       (cons 'fd/in)
                       reverse
                       (cons '(fd/domain 1 2 3 4 5 6 7 8 9))
                       reverse)
          discons `(~'fd/distinct ~varsvec)
          sumstat (cons '+ vars)
          sumcons `(~'fd/eq
                     (~'= ~n ~sumstat))
          outcons `(~'l/== ~lvar ~varsvec)]
      (reverse 
        (->> '()
             (cons 'l/fresh)
             (cons varsvec)
             (cons domcons)
             (cons discons)
             (cons sumcons)
             (cons outcons)))))
#_(macroexpand-1 '(makerel-lists 4 25 '?q))

#_(l/fresh
   [?v0 ?v1 ?v2 ?v3]
   (fd/in ?v0 ?v1 ?v2 ?v3 (fd/domain 1 2 3 4 5 6 7 8 9))
   (fd/distinct [?v0 ?v1 ?v2 ?v3])
   (fd/eq (= 25 (+ ?v0 ?v1 ?v2 ?v3)))
   (l/== ?q [?v0 ?v1 ?v2 ?v3]))

#_(defmacro makerel-getlists [k n lvar]
    (let [orig '()
          vars (vec (for [i (range k)] (symbol (str "?v" i))))
          domcons (->> (for [v vars] v)
                       (cons 'fd/in)
                       reverse
                       (cons '(fd/domain 1 2 3 4 5 6 7 8 9))
                       reverse)
          discons `(~'fd/distinct ~vars)
          sumstat (cons '+ (for [v vars] v))
          sumcons `(~'fd/eq
                     (~'= ~n ~sumstat))
          outcons `(~'l/== ~lvar ~vars)]
      (reverse (->> orig
                    (cons 'l/fresh)
                    (cons vars)
                    (cons domcons)
                    (cons discons)
                    (cons sumcons)
                    (cons outcons)))))
#_(macroexpand-1 '(makerel-getlists 2 3 '?q))

#_(into #{} 
        (map 
          set 
          (l/run* [?q] (makerel-getlists 3 9 ?q))))


(defmacro condeclause-gen [arri rulei] ;Accept the array index and the rule index
  (let [lefta (symbol (str "?c" (mod (- arri 1) 8)))
        mida  (symbol (str "?c" arri))
        righta (symbol (str "?c" (mod (+ arri 1) 8)))
        leftr (if (= (bit-and rulei 4) 4)
                1 0)
        midr  (if (= (bit-and rulei 2) 2)
                1 0)
        rightr (if (= (bit-and rulei 1) 1)
                 1 0)
        resultvar (symbol (str "?a" arri))
        rulevar   (symbol (str "?r" rulei))]
    (vec (reverse 
             (->> '()
                  (cons `(~'l/== ~lefta ~leftr))
                  (cons `(~'l/== ~mida  ~midr))
                  (cons `(~'l/== ~righta ~rightr))
                  (cons `(~'l/== ~resultvar ~rulevar)))))))

(macroexpand-1 '(condeclause-gen 7 0))

(defmacro conde-gen [arrayind]
  (let [clauses (for [#_#_arrayind (range 0 8)
                      ruleind  (range 7 -1 -1)] 
                  (macroexpand-1 `(~'condeclause-gen ~arrayind ~ruleind)))]
    (cons 'l/conde clauses)))
 
(macroexpand-1 '(conde-gen 0))


;A simple thing that iterates one step in an elementary celular automata
(defn stepo [curr aftr rule]
  (l/fresh [?c0 ?c1 ?c2 ?c3 ?c4 ?c5 ?c6 ?c7 
            ?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7
            ?r7 ?r6 ?r5 ?r4 ?r3 ?r2 ?r1 ?r0]
           (l/== curr [?c0 ?c1 ?c2 ?c3 ?c4 ?c5 ?c6 ?c7])
           (l/== rule [?r7 ?r6 ?r5 ?r4 ?r3 ?r2 ?r1 ?r0])
           (conde-gen 0)
           (conde-gen 1)
           (conde-gen 2)
           (conde-gen 3)
           (conde-gen 4)
           (conde-gen 5)
           (conde-gen 6)
           (conde-gen 7)
           (l/== aftr [?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7])))

;Generates the rule vector given a number
(defn rulegen [number]
  [(if (= 128 (bit-and number 128)) 1 0)
   (if (= 64 (bit-and number 64)) 1 0)
   (if (= 32 (bit-and number 32)) 1 0)
   (if (= 16 (bit-and number 16)) 1 0)
   (if (= 8 (bit-and number 8)) 1 0)
   (if (= 4 (bit-and number 4)) 1 0)
   (if (= 2 (bit-and number 2)) 1 0)
   (if (= 1 (bit-and number 1)) 1 0)])
   

(defn step [state rule]
  (l/run* [?q]
          (l/fresh [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7] ;Create the output vector's terms
                   (l/== ?q [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7]) ;Bind it to the output variable
                   (stepo  state ?q (rulegen rule)))))

(defn dosteps [all numsteps rule]
  (if (zero? numsteps)
    (reverse all)
   (let [curr (first all)
         aftr (first (step curr rule))]
     (dosteps (cons aftr all) (dec numsteps) rule))))

(dosteps '([0 0 0 0 0 0 0 1]) 16 110)
         
        

;Finds the rule given step constraints
(l/run* [?q]
        (l/fresh [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7] ;Create the output vector's terms
                (l/== ?q [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7]) ;Bind it to the output variable
                (stepo  [1 0 1 0 0 0 0 1] [1 1 1 1 1 1 1 1] [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7])
                (stepo  [1 1 1 1 1 1 1 1] [0 0 0 0 0 0 0 0] [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7])
                (stepo  [0 0 0 0 0 0 0 0] [1 1 1 1 1 1 1 1] [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7])))
                
                 
(l/run* [?q]
        (l/fresh [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7] ;Create the output vector's terms
                (l/== ?q [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7]) ;Bind it to the output variable
                (stepo  ?q [1 1 1 1 1 1 1 1] (rulegen 229))))

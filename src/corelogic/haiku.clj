(ns corelogic.haiku
  {:clj-kondo/config
   '{:linters {:unresolved-symbol {:exclude-patterns ["\\?\\w" "\\$\\w"]}}}}
  [:require 
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [clojure.core.logic.pldb :as pldb]])

(pldb/db-rel $pword    ?word)
(pldb/db-rel $numsylls ?word ?syllcount)
(pldb/db-rel $stresses ?word ?stresslist)

(def facts
  (pldb/db
    [$pword 'and]
    [$numsylls 'and 1]
    [$stresses 'and '(:o)]
    [$stresses 'and '(:-)]

    [$pword 'are]
    [$numsylls 'are 1]
    [$stresses 'are '(:o)]
    [$stresses 'are '(:-)]

    [$pword 'of]
    [$numsylls 'of 1]
    [$stresses 'of '(:o)]
    [$stresses 'of '(:-)]

    [$pword 'the]
    [$numsylls 'the 1]
    [$stresses 'the '(:-)]
    
    [$pword 'around]
    [$numsylls 'around 2]
    [$stresses 'around '(:- :o)]

    [$pword 'alone]
    [$numsylls 'alone 2]
    [$stresses 'alone '(:- :o)]
    
    [$pword 'very]
    [$numsylls 'very 2]
    [$stresses 'very '(:o :-)]))
    
(defn collo [coll]
  ;Ensures coll is a nonempty collection
  (l/fresh [?head ?tail]
           (l/== coll (l/lcons ?head ?tail))))

(comment
  (l/run* [?q];Should fail
          (l/== ?q 'a)
          (collo ?q)) 
  (l/run* [?q];Should also fail
          (l/== ?q [])
          (collo ?q)) 
  (l/run* [?q];Should succeed
          (l/== ?q ['a])
          (collo ?q)) 
  (l/run* [?q];Should also succeed
          (l/== ?q ['a 'b])
          (collo ?q))) 
  
(defn counto [coll len]
  (l/conde
    [(l/== coll '())
     (l/== len 0)]
    [(l/fresh [?first ?rest ?restlen]
              (l/firsto coll ?first)
              (l/resto  coll ?rest)
              (fd/+ 1 ?restlen len)
              (counto ?rest ?restlen))]))

(comment
  (l/run* [?q]
          (counto '(1 2 3 4) ?q))
  (l/run* [?q]
          (counto ?q 4)))
             

(defn syllableso [line sylls]
  (l/conde 
    ;An empty line has no syllables
    [(l/== line ())
     (l/== sylls ())]
    [(l/fresh [?firstword ?restwords ?firstsylls ?nextsylls]
              (l/firsto line ?firstword) ;Get the first word of the line
              ($stresses ?firstword ?firstsylls)
              (l/appendo sylls ?firstsylls ?nextsylls)
              (syllableso ?restwords ?nextsylls))]))

(defn fooo [bar]
  ($stresses bar))
(comment
  (l/run* [?q]
          (syllableso '('and) ?q))
  (pldb/with-db facts
    (l/run* [?q]
            ($stresses 'and ?q)))
  (pldb/with-db facts
    (l/run* [?q]
            (syllableso '('and) ?q)))
  (pldb/with-db facts
    (l/run* [?q]
            (l/conde)))
  (l/run* [?q]
          (pldb/with-db facts
            ($stresses 'and ?q))))
            
     
    
(pldb/with-db facts
  (l/run* [?q]
          (l/fresh [?l1 ?l1str
                    ?l2 ?l2str
                    ?l3 ?l3str
                    ?stresspattern]
                   ;A poem is 3 lines
                   ;(l/== ?q [?l1 ?l2 ?l3])
                   ;Of 3 iambs
                   (l/== ?stresspattern '(:- :o :- :o :- :o))
                   ;No consecutive repeated lines
                   (l/!= ?l1 ?l2)
                   (l/!= ?l2 ?l3)
                   ;All lines must have the same stresses
                   (l/== ?l1str ?stresspattern)
                   (l/== ?l2str ?stresspattern)
                   (l/== ?l3str ?stresspattern)
                   ;A line is a collection
                   ;(collo ?l1) (collo ?l2) (collo ?l3)
                   ;(collo ?l1str) (collo ?l2str) (collo ?l3str)
                   (l/== ?q ?l1str))))
                   ;Each member of each line must be a pword
                   ;(l/everyg $pword ?l1)
                   ;(l/everyg $pword ?l2)
                   ;(l/everyg $pword ?l3)
                   ;A stress pattern is made up of 
                   




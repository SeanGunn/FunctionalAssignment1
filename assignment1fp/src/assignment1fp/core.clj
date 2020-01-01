(ns assignment1fp.core)


(def q1
  ;;defining a seq to use
  lista ["a" "b" "c" "d"]
  ;;defining a seq to use
  listb ["1" "3" "1" "2"]
  ;;defining a seq to use
  listc []
  ;;defining a seq to use
  listd ["1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2"]
  ;;creating the recurive list fucntion
  (defn recur-list [listName]
    ;; turning vector into sequences
    (let list (conj listName(seq [])0)
              ;;creating the recursion
    (letfn [(sqlistsearch
              [current next sizeleft]
              ;;if size left not zero current =  next of list
              (if (zero? sizeleft)
                current
                ;;setting list value to square and also calling exp function
                (into list (exp (current current) ))
                ;;set next as current list nth + 1
                next (+ nth listName(current) 1)
                ;;finishing tail recurion by using current and next and then decrasing sizeleft by 1
                (recur next current (dec sizeleft))))]
      ;;calling sqlistsearch function
      (sqlistsearch 0 1 (count listName)))))
  ;;calling the recursive list function
  (recur-list lista)
  lista
  ;;calling the recursive list function
  (recur-list listb)
  listb
  ;;calling the recursive list function
  (recur-list listc)
  listc
  ;;calling the recursive list function
  (recur-list listd)
  listd
  )

(defn exp [x n]
  ;;for every value passed through it times them together (reduce times every value in list) (repeat returns lazy seq of values)
  (reduce * (repeat n x)))

(def change
  (loop [currentLeft v]
    (if (= 0 currentLeft)
      v
      (if (> ))))

  )



(defn conmoney [m]
  (cond
    (= \D m) 10                                             ;;dimes
    (= \Q m) 25                                             ;;quarters
    (= \N m) 5                                              ;;nickels
    (= \P m) 1                                              ;;pennies
    (= \R m) 100                                            ;;dollar
    (= \H m) 50))                                           ;;half dollar

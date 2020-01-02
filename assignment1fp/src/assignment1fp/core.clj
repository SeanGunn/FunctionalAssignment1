(ns assignment1fp.core)

;;;;;;;;;;;;;;;;;start of q1
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
;;;;;;;;;;;;end of q1

;;question 2
(defn makeChange? [currentCoinType changeLeft]
  (if (> (/ changeLeft (convertCoin currentCoinType)) 0)
    (do
      true)
    (do
      false)
    )
  )

(defn coin-changer
  [changeLeft currentCoinType coinTypeAndQuantity]
  (if (> changeLeft 0)
    (if (makeChange? currentCoinType changeLeft)
      (coin-changer
        (- change-left (currentCoinType coinTypeAndQuantity))
        currentCoinType
        (assoc
          coinTypeAndQuantity
          currentCoinType
          (inc (currentCoinType coinTypeAndQuantity)))))))

(defn convertCoin [c]
(cond
  (= :dollar c) 100
  (= :halfDollars c) 50
  (= :quarter c) 25
  (= :dime c) 10
  (= :nickel c) 5
  (= :penny c) 1
)


(coin-changer 100 :quarter {:dollar 0 :halfDollars 0 :quarter 0 :dime 0 :nickel 0 :penny 0})
(coin-changer 100 :quarter {:dollar 0 :halfDollars 0 :quarter 0 :dime 0 :nickel 0 :penny 0})
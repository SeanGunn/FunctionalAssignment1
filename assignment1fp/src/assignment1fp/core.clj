(ns assignment1fp.core)

;;;;;;;;;;;;;;;;;start of q1
(def q1

  ;;creating the recursive list function
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
                          ;;finishing tail recursion by using current and next and then deceasing sizeleft by 1
                          (recur next current (dec sizeleft))))]
                ;;calling sqlistsearch function
                (sqlistsearch 0 1 (count listName)))
              ;;printing final value
              (println list))
    )
  (defn exp [x n]
    ;;for every value passed through it times them together (reduce times every value in list) (repeat returns lazy seq of values)
    (reduce * (repeat n x)))

  ;;defining a seq to use
  ;;calling the recursive list function
  (let vectorA ["a" "b" "c" "d"](recur-list vectorA))
  (def vectorB ["1" "3" "1" "2"](recur-list vectorB))
  (def vectorC [](recur-list vectorC))
  (def vectorD ["1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2""1" "3" "1" "2"](recur-list vectorD))
  )
;;;;;;;;;;;;end of q1

;;question 2
;;Checks if their is enough money left to makeChange from either type and returns either true or false
(defn makeChange? [currentCoinType changeLeft]
  (if (pos? (/ changeLeft (convertCoin currentCoinType)))
    (do
      true)
    (do
      false)
    )
  )

(defn coinChanger
  [changeLeft currentCoinType coinTypeAndQuantity]
  ;;creating a recursive function to increase coinTypeAndQuantity when we take it away from the changeLeft
  (letfn [(coinChangerSecond
            [changeLeft currentCoinType coinTypeAndQuantity]
            ;;checks if their is a more money then 0 left
  (if (pos? changeLeft)
    ;;calls makeChange? to to see if can minus either any of the money types
    (if (makeChange? currentCoinType changeLeft)
      ;;calls its self then minus the change left by a coinType
      (coinChanger
        (- change-left (currentCoinType coinTypeAndQuantity))
        currentCoinType
        ;;associates a map key to the map
        (assoc
          coinTypeAndQuantity
          currentCoinType
          ;;inc the coinType by 1
          (inc (currentCoinType coinTypeAndQuantity))))))
            ;;checks if the money is equal to zero and prints the coinTypeAndQuantity
            (if (zero? changeLeft)
              (println coinTypeAndQuantity)))
          ;;recurs the function to lower/print the values
          (recur changeLeft currentCoinType coinTypeAndQuantity)])
  ;;calls function
  (coinChangerSecond changeLeft currentCoinType coinTypeAndQuantity)
  ;;to here
  )

(defn convertCoin [c]
  ;;reads the key and maps to the correct number using the cond statement
(cond
  (= :dollar c) 100
  (= :halfDollars c) 50
  (= :quarter c) 25
  (= :dime c) 10
  (= :nickel c) 5
  (= :penny c) 1
))


(defn Q2
  [changeLeft currentCoinType coinTypeAndQuantity outcomes]
  ;;runs the coinChanger function for the amount of outcomes possible
  ([([outcomes (size 1)]
   ;;if greater than 0 prints then calls function
          (if (pos? outcomes)
              (print "Outcome " size)
              (coinChanger changeLeft currentCoinType coinTypeAndQuantity)
            )
                           ;;finishes the tail recursion by deceasing outcomes and increasing size
          (recur (dec outcomes) (inc size)))])

  )


(Q2 100 :quarter {:dollar 0 :halfDollars 0 :quarter 0 :dime 0 :nickel 0 :penny 0} 6)
(Q2 100 :quarter {:dollar 0 :halfDollars 0 :quarter 0 :dime 0 :nickel 0 :penny 0} 6)

(defn Q4
  ((let nasa (slurp "https://data.nasa.gov/resource/y77d-th95.json"))
   (yearMostMeterFalls nasa))

  )

(defn yearMostMeterFalls [data]
  ;;defining the values to compare
  ((let yearMostCurrent (= 0))
   (let yearMostPossiable)
  (let amountCurrentMost (= 0))
   (let amountCurrentPoss (= 0))
   (let numberInList)
   ;;creating the tail recursion
      (defn yearFallsTail [n ] (
                                ;;if it has a fell as a value in the n th part of the data it will enter the if
      (if (= (nth n data (:fall)) "Fell")
        ;;if never set a value for year it will set one as base
        ((if (zero? yearMostCurrent)
           ;;setting year most current as nth data year
           ( yearMostCurrent (= (nth n data (:year))))
           ;;increasing amount
                                    (inc amountCurrentMost)
           ;;setting the refinder as n
                                    (nInList (= n)))
         )
        ;;if yearMost possible is greater than yearMostCurrent its gonna set the values to it
        (if (< yearMostCurrent yearMostPossiable)
           (= yearMostCurrent yearMostPossiable)
           (inc amountCurrentPossiable)
           (numberInList (= n)))
        ;;if it falls in year same as yearMostCurrent increase its counter
        (if (= (nth n data (:year)) yearMostCurrent)
          (inc yearMostCurrent)
          )
        ;;if it falls in year same as yearMostPossiable increase its counter
        (if (= (nth n data (:year)) yearMostPossiable)
          (inc yearMostPossiable)
          ))
                                ;;if n is the same size as the data size the call yearMostMeterFallsFinal
                                (if (= n (count data))
                    (yearMostMeterFallsFinal numberInList data)))
        (recur (inc n)))))

(defn yearMostMeterFallsFinal [numberInList data]
  ;;just prints out the year with most meterfalls
  (print "year with most MeterFalls was " (nth numberInlist data (:year)))
  )

(defn yearHeaviestMeterFall [data]
  ;;defining the values to compare
  ((let yearMostCurrent (= 0))
   (let yearMostPossiable)
   (let amountCurrentMost (= 0))
   (let amountCurrentPoss (= 0))
   (let numberInList)
   (let howHeavy1 (= 0))
   (let howHeavy2 (= 0))
   ;;creating the tail recursion
   (defn yearHeaviestTail [n ] (
                                ;;if it has a fell as a value in the n th part of the data it will enter the if
                             (if (= (nth n data (:fall)) "Fell")
                               ;;if never set a value for year it will set one as base
                               ((if (zero? yearMostCurrent)
                                  ( yearMostCurrent (= (nth n data (:year))))
                                  (inc amountCurrentMost)
                                  (nInList (= 0))
                                  (howHeavy1 (= (+ (howHeavy1 (nth n data (:mass)))))))
                                )
                               ;;if yearMostPossible is greater than yearMostCurrent its gonna set the values to it and where to find it later
                               (if (< yearMostCurrent yearMostPossiable)
                                 (= yearMostCurrent yearMostPossiable)
                                 (inc amountCurrentPossiable)
                                 (numberInList (= n))
                                 (howHeavy1 (= howHeavy2)))))
  ;;if it falls in year same as yearMostCurrent increase its counter and increases its mass
                               (if (= (nth n data (:year)) yearMostCurrent)
                                 (inc yearMostCurrent)
                                 (howHeavy1 (= (+ (howHeavy1 (nth n data (:mass)))))))
                                 )
;;if it falls in year same as yearMostPossiable increase its counter and increases its mass
                               (if (= (nth n data (:year)) yearMostPossiable)
                                 (inc yearMostPossiable)
                                 (howHeavy2 (= (+ (howHeavy2 (nth n data (:mass)))))))
;;if n is same size as the data size then calls yearHeaviestMeterFallFinal to display the answer
                                (if (= n (count data))
                                  (yearHeaviestMeterFallFinal numberInList data howHeavy1))
     (recur (inc n))))

(defn yearHeaviestMeterFallFinal [numberInList data mass]
  ;;prints out the year with the heaviest collective meteor fall
  (print "year with the heaviest MeterFalls was " (nth numberInlist data (:year)))
  (print "with a mass of "mass)
  )
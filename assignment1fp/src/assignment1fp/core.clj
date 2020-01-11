(ns assignment1fp.core)
;;ps could not test any of the code since intellj would crash if I tried to run anything (so not expecting any to be right)
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
;;;;;;;;q3
(def Q3
  ;;using random int i am adding a plant randomly to windowPlants
(( let windowPlants [] ((for [i (range 48)] (let rn(rand-int 4)
                                                 ;;if random number equals 0 adds grass
                     (if (= rn 0)
                       (do conj [g])
                       )
                                                 ;;if random number equals 1 adds clover
                     (if (= rn 1)
                       (do conj [c]))
                                                 ;;if random number equals 2 adds radishes
                     (if (= rn 2)
                       (do conj [r]))
                                                 ;;if random number equals 3 adds violets
                     (if (= rn 3)
                       (do conj [v])))))
                    ;;calling many kids with different spellings to find their plants
                    (whosPlantOnePersonAsk windowPlants "ALICE")
                    (whosPlantOnePersonAsk windowPlants "eVE")
                    (whosPlantOnePersonAsk windowPlants "Ileana")
                    (whosPlantOnePersonAsk windowPlants "larry")
                    (whosPlantOnePersonAsk windowPlants ""))
 )

(defn whosPlantOnePersonAsk [plants person]
  ;;if the kid that is passed through is the same spelling as the preset kid then they will get their plants after the person value is edit to be a lowercase
  (if (= clojure.string/lower-case person "alice")
    (do (print "Alice's plants are" (nth plants 0) (nth plants 1) (nth plants 2) (nth plants 3))))
  (if (= clojure.string/lower-case person "bob")
    (do (print "Bob's plants are" (nth plants 4) (nth plants 5) (nth plants 6) (nth plants 7) )))
  (if (= clojure.string/lower-case person "charlie")
    (do (print "Charlie's plants are" (nth plants 8) (nth plants 9) (nth plants 10) (nth plants 11))))
  (if (= clojure.string/lower-case person "david")
    (do (print "David's plants are" (nth plants 12) (nth plants 13) (nth plants 14) (nth plants 15))))
  (if (= clojure.string/lower-case person "eve")
    (do (print "Eve's plants are" (nth plants 16) (nth plants 17) (nth plants 18) (nth plants 19))))
  (if (= clojure.string/lower-case person "fred")
    (do (print "Fred's plants are" (nth plants 20) (nth plants 21) (nth plants 22) (nth plants 23))))
  (if (= clojure.string/lower-case person "ginny")
    (do (print "Ginny's plants are" (nth plants 24) (nth plants 25) (nth plants 26) (nth plants 27))))
  (if (= clojure.string/lower-case person "harriet")
    (do (print "Harriet's plants are" (nth plants 28) (nth plants 29) (nth plants 30) (nth plants 31))))
  (if (= clojure.string/lower-case person "ileana")
    (do (print "Ileana's plants are" (nth plants 32) (nth plants 33) (nth plants 34) (nth plants 35))))
  (if (= clojure.string/lower-case person "joseph")
    (do (print "Joseph's plants are" (nth plants 36) (nth plants 37) (nth plants 38) (nth plants 39))))
  (if (= clojure.string/lower-case person "kincaid")
    (do (print "Kincaid's plants are" (nth plants 40) (nth plants 42) (nth plants 43) (nth plants 44))))
  (if (= clojure.string/lower-case person "larry")
    (do (print "Larry's plants are" (nth plants 45) (nth plants 46) (nth plants 47) (nth plants 48))))
  )


;;;;;;;;q4
(defn Q4
  ((let nasa (slurp "https://data.nasa.gov/resource/y77d-th95.json"))
   (yearMostMeterFalls nasa)
   (yearHeaviestMeterFall  nasa)
   (yearMostCommon nasa)
   (greatestReclat nasa)
   (greatestReclong nasa)))


(defn yearMostMeterFalls [data]
  ;;using regex to filter through the code and get a value that, then using frequencies to par the values together so i can count then sort the values to display after
((let amount (sort (count (frequencies (filter #("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))")) ))))
 ;;calling the function to print out the top sorted answer
 (yearMostMeterFallsAnswer (first amount))
  )

  (defn yearMostMeterFallsAnswer [year]
  ;;just prints out the year with most meterfalls
  (print "year with most MeterFalls was " year )
  )

(defn yearHeaviestMeterFall [data]
  ;;using regex to filter through the code and get a value that, then using frequencies to par the values together then sorting by date to get them in order then count the values together to add them
  ((let meterFallSorted (count (sort-by :date (frequencies (filter #("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))")))))))
   ;;calling the function to print out the top sorted answer
   (yearHeaviestMeterFallFinal (first meterFallSorted))))



(defn yearHeaviestMeterFallFinal [numberInList]
  ;;prints out the year with the heaviest collective meteor fall
  (print "year with the heaviest MeterFalls was " numberInList))

(defn yearMostCommon [data]
  ;;using regex to filter through the code and get a value that, then using frequencies to par the values together then sorting them by the most common date to display afterwards
  ((let yearMostCommonSorted (sort-by :date (frequencies (filter #("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))"))))))
  ;;calling the function to print out the top sorted answer
  (yearMostCommonFinal (first yearMostCommonSorted))
  )


(defn yearMostCommonFinal [numberInList]
  ;;prints out the year with the heaviest collective meteor fall
  (print "the year that was the most common was " numberInList))

(defn greatestReclat [data]
  ;;using regex to filter through the code and get a value that, then using frequencies to par the values together then sorting them by the greatest reclat value to display afterwards
  ((let greatestReclatSorted (sort-by > val (frequencies (filter #("[\"]\\w{6}[\"][:][\"][-]?\\d+.\\d\\d\\d\\d\\d\\d"))))))
  ;;calling the function to print out the top sorted answer
  (yearGreatestReclatFinal (first greatestReclatSorted))
  )

(defn yearGreatestReclatFinal [numberInList]
  ;;prints out the greatest reclate
  (print "the greatest reclat was " numberInList))

(defn greatestReclong [data]
  ;;using regex to filter through the code and get a value that, then using frequencies to par the values together then sorting them by the greatest reclong value to display afterwards
  ((let greatestReclongSorted (sort-by > val (frequencies (filter #("[\"]\\w{7}[\"][:][\"][-]?\\d+.\\d\\d\\d\\d\\d\\d"))))))
  ;;calling the function to print out the top sorted answer
  (yearGreatestReclongFinal (first greatestReclongSorted))
  )

(defn yearGreatestReclongFinal [numberInList]
  ;;prints out the greatest reclong
  (print "the greatest reclong was " numberInList))
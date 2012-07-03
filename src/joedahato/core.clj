(ns joedahato.core
  (:require [incanter.core :as ic]
            [incanter.stats :as stats])
  (:use clj-time.core
        clj-time.format))

(def observations2
  {"4/25/2012" "off-white"
   "4/26/2012" "black/black"
   "4/27/2012" "red/white"
   "6/4/2012"  "red/white"
   "6/5/2012"  "red/white"
   "6/6/2012"  "sky blue/white"
   "6/8/2012"  "teal/white"
   "6/11/2012" "light grey/black"
   "6/12/2012" "red/white"
   "6/13/2012" "no hat"
   "6/14/2012" "teal/white"
   "6/15/2012" "red/white"
   "6/18/2012" "red/white"
   "6/19/2012" "light grey/black"
   "6/21/2012" "sky blue/white"
   "6/22/2012" "teal/white"
   "6/25/2012" "no hat"
   "6/26/2012" "black/black"
   "6/27/2012" "light grey/black"
   "7/2/2012"  "red/white"})

(def fmt (formatter "MM/dd/yyyy"))

(def observations2 
  (into {} (for [[date hat] observations2]
                [(parse fmt date) hat])))

(defn times 
  [observations]
  (sort (keys observations)))

(def hats (doall (vec (distinct (vals observations2)))))

(def hat-number 
  (into {} (map-indexed (fn [x y] [y x]) hats)))

(defn hat-vector
  "Given a hat string, returns the state vector."
  [hatname]
  (if hatname
    (assoc (vec (repeat (count hats) 0))
           (hat-number hatname) 1)
    (vec (repeat (count hats) (/ 1 (count hats))))))

(defn interpret
  "Given a hat vector, describe the hat."
  [vector]
  (let [thoughts (keep identity
                    (map-indexed
                      (fn [i p]
                        (when (< (/ 1 (count hats)) p)
                          (if (= p 1)
                            (str "Definitely " (hats i))
                            (str (format "%.3f" p) " " (hats i)))))
                      vector))]
    (if (empty? thoughts)
      "dunno"
      (apply str (interpose ", " thoughts)))))

(defn known
  "Returns a vector of known information at time t."
  [observations t]
  (let [t-1 (minus t (days 1))
        t-2 (minus t (days 2))]
    (concat [1]
      (hat-vector (observations t-1)))))

(defn actual
  "Returns a vector of the actual hat at time t."
  [observations t]
  (hat-vector (observations t)))

(defn all-known 
  "A matrix of the known vectors for each given time."
  [observations]
  (ic/trans (ic/matrix (map (partial known observations)
                            (times observations)))))

(defn all-actual
  "A matrix of the actual vectors for each given time."
  [observations]
  (ic/trans (ic/matrix (map (partial actual observations)
                            (times observations)))))

(defn one-day-model
  "A model for the given set of observations, taking into account the previous
  day's hat only."
  [observations]
  (let [X (all-actual observations)
        K (all-known observations)
        Kt (ic/trans K)]
    (ic/mmult X Kt
              (ic/trans
                (ic/solve
                  (ic/mmult K Kt))))))

(defn predict
  "A prediction vector from model and known priors."
  [model known]
  (ic/mmult model known))

(defn before
  "Observations up to but not including cutoff time"
  [observations cutoff]
  (into {} (for [[t val] observations :when (before? t cutoff)] [t val])))

(defn accuracy
  "Evaluates the accuracy of the model fn over observations."
  [model observations]
  (stats/mean
    (keep identity
      (for [t (take-last 10 (times observations))]
        (try
          (let [cut-observations (before observations t)
                m (model cut-observations)
                actual (actual observations t)
                predicted (predict m (known observations t))
                p (reduce + (map * actual predicted))]
  ;            (println t)
  ;            (println "Predicted" (interpret predicted))
  ;            (println "Actual" (interpret actual))
  ;            (println p)
            p)
          (catch Exception e
            nil))))))

(defn -main
  "YYYYEAaaahahhHHhHHhHhh"
  [& args]
    (println "Average accuracy over last 10 days" (accuracy one-day-model observations2))
  (let [m (one-day-model observations2)
        p (predict m (known observations2 (parse fmt (first args))))]
        (println "Prediction for" (first args) "-" (interpret p))))

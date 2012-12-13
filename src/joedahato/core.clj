(ns joedahato.core
  (:require [incanter.core :as ic]
            [incanter.stats :as stats])
  (:use [clojure.pprint :only [pprint]]
        clj-time.core
        clj-time.format
        clj-ml.data
        clj-ml.classifiers))

(def observations
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
   "7/2/2012"  "red/white"
   "7/3/2012"  "teal/white"
   "7/5/2012"  "black/black"
   "7/6/2012"  "red/white"
   "7/9/2012"  "black/black"
   "7/10/2012" "sky blue/white"
   "7/12/2012" "black/black"
   "7/13/2012" "light grey/black"
   "7/24/2012" "black/white"
   "7/25/2012" "sky blue/white"
   "7/26/2012" "black/white"
   "7/27/2012" "black/black"
   "7/30/2012" "red/white"
   "8/6/2012"  "red/white"
   "8/7/2012"  "light grey/black"
   "8/8/2012"  "black/white"
   "8/9/2012"  "black/white"
   "8/10/2012" "black/white"
   "8/14/2012" "no hat"
   "8/15/2012" "light grey/black"
   "8/16/2012" "black/white"
   "8/17/2012" "red/white"
   "8/21/2012" "light grey/black"
   "8/29/2012" "no hat"
   "8/30/2012" "no hat"
   "8/31/2012" "no hat"
   "9/4/2012"  "red/white"
   "9/5/2012"  "black/white"
   "9/6/2012"  "teal/white"
   "9/7/2012"  "light grey/black"
   "9/18/2012" "black/white"
   "9/20/2012" "light grey/black"
   "9/21/2012" "sky blue/white"
   "9/25/2012" "teal/white"
   "9/27/2012" "sky blue/white"
   "9/28/2012" "light grey/black"
   "10/2/2012"  "sky blue/white"
   "10/3/2012"  "red/white"
   "10/4/2012"  "no hat"
   "10/5/2012"  "teal/white"
   "10/8/2012"  "sky blue/white"
   "10/9/2012"  "teal/white"
   "10/12/2012" "light grey/black"
   "10/17/2012" "black/white"
   "10/18/2012" "black/white"})

(def fmt (formatter "MM/dd/yyyy"))

(def observations 
  (into {} (for [[date hat] observations]
                [(parse fmt date) hat])))

(def weekdays [:mon :tue :wed :thu :fri :sat :sun])

(defn hats
  "Hats in observations"
  [observations]
  (doall (vec (sort (distinct (vals observations))))))

(defn times
  [observations]
  (sort (keys observations)))

(defn ds-row
  "The row for the given set of observations, at time t"
  [obs t]
  (let [previous-days (take-while pos? (iterate dec 4))]
    (concat 
      ; Today
      [(obs t)]
      ; Previous days
      (map (fn [ago] (obs (minus t (days ago)))) previous-days)
      ; Day of week
      [(weekdays (dec (day-of-week t)))]
      )))

(defn ds [observations]
  (let [hats (hats observations)
        ds   (make-dataset 
               "hats" 
               [{:today hats}
;                {:prev7 hats} 
;                {:prev6 hats} 
;                {:prev5 hats} 
                {:prev4 hats} 
                {:prev3 hats} 
                {:prev2 hats} 
                {:prev1 hats}
                {:day-of-week weekdays}]
               (map (partial ds-row observations)
                    (times observations)))]
    (dataset-set-class ds 0)
    ds))

(defn classifier 
  ([ds] (classifier ds :neural-network :multilayer-perceptron))
  ([ds o1 o2]
   (let [classifier (make-classifier o1 o2)]
     (classifier-train classifier ds)
     classifier)))

(defn before
  "Observations up to but not including cutoff time"
  [observations cutoff]
  (into {} (for [[t val] observations :when (before? t cutoff)] [t val])))

(defn predict 
  [observations ds classifier t]
  (let [instance (make-instance ds (ds-row observations t))
        classes (into {} (for [[k v] (dataset-class-labels ds)] [v k]))]
    (classes (long (classifier-classify classifier instance)))))

;(defn accuracy
;  "Evaluates the accuracy of the model fn over observations."
;  [model observations]
;  (stats/mean
;    (keep identity
;      (for [t (take-last 10 (times observations))]
;        (try
;          (let [cut-observations (before observations t)
;                m (model cut-observations)
;                actual (actual observations t)
;                predicted (predict m (known observations t))
;                p (reduce + (map * actual predicted))]
;  ;            (println t)
;  ;            (println "Predicted" (interpret predicted))
;  ;            (println "Actual" (interpret actual))
;  ;            (println p)
;            p)
;          (catch Exception e
;            nil))))))

(defn -main
  "YYYYEAaaahahhHHhHHhHhh"
  [& args]
  (let [ds (ds observations)
        cl (classifier ds)
        ev (classifier-evaluate cl :dataset ds ds)]
    (println "kappa" (:kappa ev))
    (println "rms" (:root-mean-squared-error ev))
    (println "precision" (:precision ev))
;    (pprint ev)
    (prn (predict observations ds cl (parse fmt (first args))))
    ))

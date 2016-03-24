(ns car-factory.common
  (:import [java.util.concurrent ThreadLocalRandom]))

(defrecord CarPart [sn type defective]
  Object
  (toString [_]
    (str (condp = type
           :engine "Engine["
           :wheel "Wheel["
           :coachwork "Coachwork[") "sn=" sn ", defective=" defective "]")))

(defrecord Car [sn engine coachwork wheels color]
  Object
  (toString [_]
    (str "Car[sn=" sn ", engine=" engine ", coachwork=" coachwork ", wheels=" wheels ", color=" color "]")))

(def engine-defective-prob 0.1)
(def coachwork-defective-prob 0.1)
(def wheel-defective-prob 0.1)
(def wheels-per-car 4)
(def million 1000000)

(defn not-defective? [car-part]
  (not (:defective car-part)))

(defn next-sn [sn-counter]
  (swap! sn-counter inc))

(defn tlrand-boolean [probability-true]
  (>= (.. ThreadLocalRandom (current) (nextDouble)) (- 1.0 probability-true)))

(defn tlrand-nth [coll]
  (nth coll (.. ThreadLocalRandom (current) (nextInt (count coll)))))

(defn print-every-millionth-car [n car]
  (when (= 0 (mod n million))
    (do
      (printf "%3d M: %s\n" (/ n million) car)
      (flush))))

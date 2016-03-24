(ns car-factory.naive-solution
  (:require [clojure.core.async :refer [go-loop chan <! >! mix admix close! <!!]])
  (:import [java.util.concurrent ThreadLocalRandom]))

(defrecord CarPart [sn type defective]
  Object
  (toString [_]
    (str (condp = type
           :engine "Engine["
           :wheel "Wheel["
           :coachwork "Coachworks[") "sn=" sn ", defective=" defective "]")))

(defrecord Car [sn engine coachworks wheels color]
  Object
  (toString [_]
    (str "Car[sn=" sn ", engine=" engine ", coachworks=" coachworks ", wheels=" wheels ", color=" color "]")))

(def engine-defectivity-prob 0.1)
(def coachworks-defectivity-prob 0.1)
(def wheel-defectivity-prob 0.1)

(defn- not-defective? [car-part]
  (not (:defective car-part)))

(defn- next-sn [sn-counter]
  (swap! sn-counter inc))

(defn- tlrand-defective [probability]
  (<= (.. ThreadLocalRandom (current) (nextDouble)) probability))

(defn- tlrand-nth [coll]
  (nth coll (.. ThreadLocalRandom (current) (nextInt (count coll)))))

(defn- produce-car-parts
  [dst-belt sn-counter car-part-type defective-probability]
  (go-loop [sn (next-sn sn-counter)]
    (when
      (>! dst-belt (->CarPart sn car-part-type (tlrand-defective defective-probability)))
      (recur (next-sn sn-counter)))))

(defn produce-engines [dst-belt sn-counter]
  (produce-car-parts dst-belt sn-counter :engine engine-defectivity-prob))

(defn produce-coachworks [dst-belt sn-counter]
  (produce-car-parts dst-belt sn-counter :coachwork coachworks-defectivity-prob))

(defn produce-wheels [dst-belt sn-counter]
  (produce-car-parts dst-belt sn-counter :wheel wheel-defectivity-prob))

(defn filter-defective [src-belt dst-belt]
  (go-loop [car-part (<! src-belt)]
    (when (some? car-part)
      (do
        (when (not-defective? car-part)
          (>! dst-belt car-part))
        (recur (<! src-belt))))))

(defn assemble-car [sn-counter engine-belt coachworks-belt wheels-belt dst-belt]
  (go-loop [engine (<! engine-belt) coachworks (<! coachworks-belt)
            wheels (repeatedly 4 #(<!! wheels-belt))]
    (when (every? some? (into [] (concat [engine coachworks] wheels)))
      (do
        (>! dst-belt (->Car (next-sn sn-counter) engine coachworks wheels :no-color))
        (recur (<! engine-belt) (<! coachworks-belt) (repeatedly 4 #(<!! wheels-belt)))))))

(defn select-paint [src-belt r-belt g-belt b-belt]
  (go-loop [car (<! src-belt)]
    (when (some? car)
      (do
        (>! (tlrand-nth [r-belt g-belt b-belt]) car)
        (recur (<! src-belt))))))

(defn paint-car [src-belt dst-belt color]
  (go-loop [car (<! src-belt)]
    (when (some? car)
      (do
        (>! dst-belt (assoc car :color color))
        (recur (<! src-belt))))))

(defn merge-painted [r-belt g-belt b-belt dest-belt]
  (let [m (mix dest-belt)]
    (doall (for [b [r-belt g-belt b-belt]] (admix m b)))))

(defn print-if-anniversary [n car]
  (when (= 0 (mod n 50000))
    (do
      (printf "Car number %9d: %s\n" n car)
      (flush))))

(defn production-counter [out-belt]
  (go-loop [car (<! out-belt) n 0]
    (if (some? car)
      (do
        (print-if-anniversary n car)
        (recur (<! out-belt) (inc n)))
      n)))

(comment
  (defn -main
    [duration]
    (let [t (Integer. ^String duration)
          [engine->fengine coachworks->fcoachworks wheels->fwheels
           fengine->assembly fcoachworks->assembly fwheels->assembly
           assembly->splitter splitter->rpaint splitter->gpaint splitter->bpaint
           rpaint->merger gpaint->merger bpaint->merger merger->counter
           :as channels] (repeatedly 14 chan)
          sn-counter (atom 0)
          result-chan (production-counter merger->counter)]
      (do (produce-engines engine->fengine sn-counter)
          (produce-coachworks coachworks->fcoachworks sn-counter)
          (produce-wheels wheels->fwheels sn-counter)
          (filter-defective engine->fengine fengine->assembly)
          (filter-defective coachworks->fcoachworks fcoachworks->assembly)
          (filter-defective wheels->fwheels fwheels->assembly)
          (assemble-car sn-counter fengine->assembly fcoachworks->assembly fwheels->assembly assembly->splitter)
          (select-paint assembly->splitter splitter->rpaint splitter->gpaint splitter->bpaint)
          (paint-car splitter->rpaint rpaint->merger :red)
          (paint-car splitter->gpaint gpaint->merger :green)
          (paint-car splitter->bpaint bpaint->merger :blue)
          (merge-painted rpaint->merger gpaint->merger bpaint->merger merger->counter)
          (Thread/sleep (* 1000 t))
          (doall (for [c channels] (close! c)))
          (println (str "Cars/second: " (/ (<!! result-chan) (double t))))))))
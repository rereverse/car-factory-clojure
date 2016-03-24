(ns car-factory.core
  (:require [clojure.core.async :as a])
  (:import [java.util.concurrent ThreadLocalRandom TimeUnit])
  (:gen-class))

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
(def buf-size 50)
(def wheels-per-car 4)
(def million 1000000)

(defn- not-defective? [car-part]
  (not (:defective car-part)))

(defn- next-sn [sn-counter]
  (swap! sn-counter inc))

(defn- make-painter [color]
  (fn [car] (assoc car :color color)))

(defn- tlrand-boolean [probability-true]
  (>= (.. ThreadLocalRandom (current) (nextDouble)) (- 1.0 probability-true)))

(defn- tlrand-nth [coll]
  (nth coll (.. ThreadLocalRandom (current) (nextInt (count coll)))))

(defn- rand-rgb [_]
  (tlrand-nth [:red :green :blue]))

(defn- produce-car-parts
  [dst-belt sn-counter car-part-type defective-probability]
  (a/go-loop [sn (next-sn sn-counter)]
    (when
      (a/>! dst-belt (->CarPart sn car-part-type (tlrand-boolean defective-probability)))
      (recur (next-sn sn-counter)))))

(defn- print-if-anniversary [n car]
  (when (= 0 (mod n million))
    (do
      (printf "%3d M: %s\n" (/ n million) car)
      (flush))))

(defn assemble-car [sn-counter engine-belt coachwork-belt wheels-belt]
  (let [out (a/chan buf-size)]
    (a/go-loop [engine (a/<! engine-belt) coachwork (a/<! coachwork-belt) wheels (a/<! wheels-belt)]
      (when (and (some? engine) (some? coachwork) (= wheels-per-car (count wheels)))
        (do
          (a/>! out (->Car (next-sn sn-counter) engine coachwork wheels :no-color))
          (recur (a/<! engine-belt) (a/<! coachwork-belt) (a/<! wheels-belt)))))
    out))

(defn production-counter [out-belt]
  (a/reduce (fn [n car] (print-if-anniversary n car) (inc n)) 0 out-belt))

(defn -main
  [duration]
  (let [t (Integer. ^String duration)
        sn-counter (atom 0)
        engines (a/chan buf-size (filter not-defective?))
        coachworks (a/chan buf-size (filter not-defective?))
        wheels (a/chan buf-size (comp (partition-all wheels-per-car) (filter not-defective?)))
        assembled-cars (assemble-car sn-counter engines coachworks wheels)
        pub-assembled-cars (a/pub assembled-cars rand-rgb (fn [_] buf-size))
        red-cars (a/chan buf-size (map (make-painter :red)))
        green-cars (a/chan buf-size (map (make-painter :green)))
        blue-cars (a/chan buf-size (map (make-painter :blue)))
        painted-cars (a/merge [red-cars green-cars blue-cars] buf-size)
        result-chan (production-counter painted-cars)]
    (do
      (a/sub pub-assembled-cars :red red-cars)
      (a/sub pub-assembled-cars :green green-cars)
      (a/sub pub-assembled-cars :blue blue-cars)
      (produce-car-parts engines sn-counter :engine engine-defective-prob)
      (produce-car-parts coachworks sn-counter :coachwork coachwork-defective-prob)
      (produce-car-parts wheels sn-counter :wheel wheel-defective-prob)
      (Thread/sleep (.toMillis TimeUnit/SECONDS t))
      (doall (for [c [engines coachworks wheels assembled-cars red-cars green-cars blue-cars painted-cars]]
               (a/close! c)))
      (println (str "Cars/second: " (/ (a/<!! result-chan) (double t)))))))
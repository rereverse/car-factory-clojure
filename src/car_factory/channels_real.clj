(ns car-factory.channels-real
  (:require [clojure.core.async :as a])
  (:use [car-factory.common])
  (:import [java.util.concurrent TimeUnit])
  (:gen-class))

(def buf-size 50)

(defn- make-painter [color]
  (fn [car] (assoc car :color color)))

(defn- rand-rgb [_]
  (tlrand-nth [:red :green :blue]))

(defn produce-car-parts
  [dst-belt sn-counter car-part-type defective-probability]
  (a/go-loop [sn (next-sn sn-counter)]
    (when
      (a/>! dst-belt (->CarPart sn car-part-type (tlrand-boolean defective-probability)))
      (recur (next-sn sn-counter)))))

(defn assemble-car [sn-counter engine-belt coachwork-belt wheels-belt]
  (let [out (a/chan buf-size)]
    (a/go-loop [engine (a/<! engine-belt) coachwork (a/<! coachwork-belt) wheels (a/<! wheels-belt)]
      (when (and (some? engine) (some? coachwork) (= wheels-per-car (count wheels)))
        (do
          (a/>! out (->Car (next-sn sn-counter) engine coachwork wheels :no-color))
          (recur (a/<! engine-belt) (a/<! coachwork-belt) (a/<! wheels-belt)))))
    out))

(defn production-counter [out-belt]
  (a/reduce (fn [n car] (print-every-millionth-car n car) (inc n)) 0 out-belt))

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

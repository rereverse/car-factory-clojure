(ns car-factory.channels-naive
  (:require [clojure.core.async :as a])
  (:import [java.util.concurrent TimeUnit])
  (:use [car-factory.common])
  (:gen-class))

(defn produce-car-parts
  [dst-belt sn-counter car-part-type defective-probability]
  (a/go-loop [sn (next-sn sn-counter)]
    (when
      (a/>! dst-belt (->CarPart sn car-part-type (tlrand-boolean defective-probability)))
      (recur (next-sn sn-counter)))))

(defn filter-defective [src-belt dst-belt]
  (a/go-loop [car-part (a/<! src-belt)]
    (when (some? car-part)
      (do
        (when (not-defective? car-part)
          (a/>! dst-belt car-part))
        (recur (a/<! src-belt))))))

(defn assemble-car [sn-counter engine-belt coachworks-belt wheels-belt dst-belt]
  (a/go-loop [engine (a/<! engine-belt) coachwork (a/<! coachworks-belt)
              wheels (repeatedly wheels-per-car #(a/<!! wheels-belt))]
    (when (every? some? (concat [engine coachwork] wheels))
      (do
        (a/>! dst-belt (->Car (next-sn sn-counter) engine coachwork (vec wheels) :no-color))
        (recur (a/<! engine-belt) (a/<! coachworks-belt) (repeatedly wheels-per-car #(a/<!! wheels-belt)))))))

(defn select-paint [src-belt r-belt g-belt b-belt]
  (a/go-loop [car (a/<! src-belt)]
    (when (some? car)
      (do
        (a/>! (tlrand-nth [r-belt g-belt b-belt]) car)
        (recur (a/<! src-belt))))))

(defn paint-car [src-belt dst-belt color]
  (a/go-loop [car (a/<! src-belt)]
    (when (some? car)
      (do
        (a/>! dst-belt (assoc car :color color))
        (recur (a/<! src-belt))))))

(defn merge-painted [r-belt g-belt b-belt dest-belt]
  (let [m (a/mix dest-belt)]
    (doall (for [b [r-belt g-belt b-belt]] (a/admix m b)))))

(defn production-counter [out-belt]
  (a/go-loop [car (a/<! out-belt) n 0]
    (if (some? car)
      (do
        (print-every-millionth-car n car)
        (recur (a/<! out-belt) (inc n)))
      n)))

(defn -main
  [duration]
  (let [t (Integer. ^String duration)
        [engines->fengines coachworks->fcoachworks wheels->fwheels
         fengines->assembly fcoachworks->assembly fwheels->assembly
         assembly->splitter splitter->rpaint splitter->gpaint splitter->bpaint
         rpaint->merger gpaint->merger bpaint->merger merger->counter
         :as channels] (repeatedly 14 a/chan)
        sn-counter (atom 0)
        result-chan (production-counter merger->counter)]
    (do (produce-car-parts engines->fengines sn-counter :engine engine-defective-prob)
        (produce-car-parts coachworks->fcoachworks sn-counter :coachwork coachwork-defective-prob)
        (produce-car-parts wheels->fwheels sn-counter :wheel wheel-defective-prob)
        (filter-defective engines->fengines fengines->assembly)
        (filter-defective coachworks->fcoachworks fcoachworks->assembly)
        (filter-defective wheels->fwheels fwheels->assembly)
        (assemble-car sn-counter fengines->assembly fcoachworks->assembly fwheels->assembly assembly->splitter)
        (select-paint assembly->splitter splitter->rpaint splitter->gpaint splitter->bpaint)
        (paint-car splitter->rpaint rpaint->merger :red)
        (paint-car splitter->gpaint gpaint->merger :green)
        (paint-car splitter->bpaint bpaint->merger :blue)
        (merge-painted rpaint->merger gpaint->merger bpaint->merger merger->counter)
        (Thread/sleep (.toMillis TimeUnit/SECONDS t))
        (doall (for [c channels] (a/close! c)))
        (println (str "Cars/second: " (/ (a/<!! result-chan) (double t)))))))

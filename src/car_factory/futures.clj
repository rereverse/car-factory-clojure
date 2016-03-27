(ns car-factory.futures
  (:require [clojure.core.async :as a])
  (:use car-factory.common)
  (:import [java.util.concurrent TimeUnit])
  (:gen-class))

(defn produce-non-def-cp
  [sn-counter car-part-type defective-prob]
  (future
    (loop [cp (->CarPart (next-sn sn-counter) car-part-type (tlrand-boolean defective-prob))]
      (if (not-defective? cp)
        cp
        (recur (->CarPart (next-sn sn-counter) car-part-type (tlrand-boolean defective-prob)))))))

(defn assemble-car [sn-counter engine coachwork wheels]
  (future (->Car (next-sn sn-counter) engine coachwork wheels :no-color)))

(defn paint-with-randm-color [car]
  (future (assoc car :color (tlrand-nth [:red :green :blue]))))

(defn rollout-car [sn-counter]
  (let [engine (produce-non-def-cp sn-counter :engine engine-defective-prob)
        coachwork (produce-non-def-cp sn-counter :coachwork coachwork-defective-prob)
        wheels (doall
                 (repeatedly
                   wheels-per-car
                   #(produce-non-def-cp sn-counter :wheel wheel-defective-prob)))
        car (assemble-car sn-counter @engine @coachwork (mapv deref wheels))
        pcar (paint-with-randm-color @car)]
    @pcar))

(defn produce-cars [sn-counter car-counter stop]
  (a/go-loop [should-go (not @stop)]
    (when should-go
      (do
        (print-every-millionth-car (swap! car-counter inc) (rollout-car sn-counter))
        (recur (not @stop))))))

(defn -main
  [duration]
  (let [t (Integer. ^String duration)
        sn-counter (atom 0)
        car-counter (atom 0)
        stop (volatile! false)]
    (do
      (println "Futures")
      (doall (repeatedly (.availableProcessors (Runtime/getRuntime)) #(produce-cars sn-counter car-counter stop)))
      (Thread/sleep (.toMillis TimeUnit/SECONDS t))
      (vreset! stop true)
      (println (str "Cars/second: " (/ @car-counter (double t)))))))

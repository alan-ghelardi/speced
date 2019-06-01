(ns speced.core-test
  (:refer-clojure :exclude [inc])
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [speced.core :as speced]
            [speced.people :as people]))

(defn ranged-rand
  "Returns random int in range start <= rand < end."
  [start end]
  (+ start (long (rand (- start end)))))

(s/fdef ranged-rand
  :args (s/and (s/cat :start int? :end int?)
               #(< (:start %) (:end %)))
  :ret int?
  :fn (s/and #(>= (:ret %) (-> % :args :start))
             #(< (:ret %) (-> % :args :end))))

(defn inc [x]
  (+ x 1))

(s/fdef inc
  :args (s/cat :x number?)
  :ret number?)

(defn hello [person]
  (throw (Exception. "Boom!")))

(s/fdef hello
  :args (s/cat :person string?)
  :ret string?)

(speced/gen-test people/get-person)

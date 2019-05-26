(ns speced.people
  (:require [clojure.spec.alpha :as s]))

(s/def ::first-name string?)
(s/def ::person (s/keys :req-un [::first-name]))

(defn get-person [id]
  {})

(s/fdef get-person :args (s/cat :id int?)
  :ret ::person)

(ns speced.colors
  (:require [clojure.pprint :as pprint])
  (:import java.io.Writer))

(def ^:private ansi-codes {:green "\u001B[32m"
                           :red   "\u001B[31m"
                           :reset "\u001B[m"})

(defmacro with-colorized-output [color ^Writer writer & body]
  `(let [ignore# (.write ~writer (~color ansi-codes))
         result# (do ~@body)]
     (.write ~writer (:reset ansi-codes))
     result#))

(defrecord Tag [color forms])

(defmethod print-method Tag [tag ^Writer writer]
  (with-colorized-output (:color tag) writer
    (print-method (:forms tag) writer)))

(defmethod pprint/simple-dispatch Tag [tag]
  (with-colorized-output (:color tag) *out*
    (pprint/simple-dispatch (:forms tag))))

(defn- tag [color forms]
  (map->Tag {:color color :forms forms}))

(def green (partial tag :green))

(def red (partial tag :red))

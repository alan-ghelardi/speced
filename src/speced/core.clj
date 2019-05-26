(ns speced.core
  (:require [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as spec.test]
            [clojure.test :as clojure.test]
            [clojure.walk :as walk])
  (:import clojure.lang.Symbol
           java.io.Writer))

(alias 'test.check 'clojure.spec.test.check)

(defonce ^{:dynamic true}
  *check-options* nil)

(defn- alias-for-ns [ns]
  #nu/tap *check-options*
  (some-> *check-options*
          :test-ns
          ns-aliases
          set/map-invert
          (get ns)))

(defn- aliased-fn-symbol [^Symbol sym]
  (let [ns      (the-ns (symbol (namespace sym)))
        fn-name (name sym)]
    (symbol
     (if-let [alias (alias-for-ns ns)]
       (str alias "/" fn-name)
       fn-name))))

(defn- describe-fn-call [{:keys [failure sym]}]
  (->> failure
       ex-data
       ::s/problems
       first
       :val
       :args
       vals
       (cons (aliased-fn-symbol sym))))

(defn- args-and-ret [{:keys [failure]}]
  (->> failure
       ex-data
       ::s/problems
       first
       :val))

(defn- describe-actual [{:keys [failure ::test.check/ret] :as result}]
  (let [{:keys [failed-after-ms seed]} ret
        {:keys [args ret]}             (args-and-ret result)]
    {:fail         (list '= ret (describe-fn-call result))
     :named-args   args
     :return-value ret
     :failed-after (str failed-after-ms "ms")
     :seed         seed
     :problems     (-> failure ex-data ::s/problems)}))

(defrecord AnonymousFunction [form])

(defmethod print-method AnonymousFunction [x ^Writer writer]
  (.write writer "#")
  (print-method (:form x) writer))

(defmethod pprint/simple-dispatch AnonymousFunction [anonymous-fn]
  (.write *out* "#")
  (pprint/simple-dispatch (:form anonymous-fn)))

(def ^:private argument-re #"^p(\d)__.*#$")

(defn- describe-argument [arg]
  (or (some->> (name arg)
               (re-find argument-re)
               last
               (str "%")
               symbol)
      arg))

(defn- describe-anonymous-function [form]
  (map->AnonymousFunction {:form
                           (walk/postwalk #(if (symbol? %)
                                             (describe-argument %)
                                             %)
                                          (last form))}))

(defn- anonymous-function? [form]
  (and (seq? form) (= 'fn* (first form))))

(defn describe-fspec [fspec]
  (walk/postwalk (fn [form]
                   (if (anonymous-function? form)
                     (describe-anonymous-function form)
                     form))
                 (s/describe fspec)))

(defn- describe-expected [{:keys [failure spec sym] :as result}]
  (let [fn-form (if failure
                  (describe-fn-call result)
                  (aliased-fn-symbol sym))]
    (list 'conforms? (describe-fspec spec)
          fn-form)))

(defn describe-result [{:keys [failure] :as result}]
  (let [expected (describe-expected result)]
    (if failure
      {:type :fail :expected expected :actual (describe-actual result)}
      {:type :pass :expected expected :actual true})))

(defn check [fn-symbol]
  (first (spec.test/check fn-symbol)))

(declare conforms?)

(defmethod clojure.test/assert-expr 'conforms? [message form]
  `(let [fn-symbol# ~(last form)
        result# (check fn-symbol#)]
    (clojure.test/do-report (assoc (describe-result result#)
                                   :message ~message))))

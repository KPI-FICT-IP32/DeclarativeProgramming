(ns anxmatrix.core
  (:require [clojure.core :as cc])
  (:require [clojure.math.combinatorics :as combo])
  (:import java.lang.UnsupportedOperationException))

(defn- anxmatrix.core/inv [left right n]
  (loop [l left r right out [] inv n]
    (if (or (empty? l) (empty? r))
      {:seq (concat out l r) :inv inv}
      (let [x (first l) y (first r)]
        (if (<= x y)
          (recur (rest l) r (conj out x) inv)
          (recur l (rest r) (conj out y) (+ inv (count (filter #(> % y) l)))))))))

(defn anxmatrix.core/count-inv
  ([coll] (count-inv coll 0))
  ([coll n]
   (if (> (count coll) 1)
     (let [[left right] (split-at (quot (count coll) 2) coll)
           l (count-inv left n)
           r (count-inv right n)]
       (inv (:seq l) (:seq r) (+ (:inv l) (:inv r))))
     {:seq coll :inv n})))

(defn anxmatrix.core/transpose [s] (apply mapv vector s))

(defn anxmatrix.core/nested-for [f x y] (mapv (fn [a] (mapv (fn [b] (f a b)) y)) x))

(defn anxmatrix.core/matrix-mult [a b] (nested-for (fn [x y] (reduce + (map * x y))) a (transpose b)))

(defn anxmatrix.core/matrix-add [a b] (mapv (fn [ai bi] (mapv + ai bi)) a b))

(defn anxmatrix.core/matrix-substract [a b] (mapv (fn [ai bi] (mapv - ai bi)) a b))

(defn anxmatrix.core/matrix? [x]
  (true?
    (and
      (vector? x)                                           ;; vector
      (seq x)                                               ;; not empty
      (every? vector? x)                                    ;; consists of vectors
      (every? (fn [xi] (every? number? xi)) x)              ;; which consists of numbers
      (every? seq x)                                        ;; and not empty too
      (apply = (map count x))                               ;; and have equal lengths
      )))

(defn anxmatrix.core/matrix [x]
  (cond
    (matrix? x) x
    (and (coll? x) (seq x) (every? number? x)) (vector (vec x))
    (and (coll? x) (every? coll? x)) (if (matrix? (mapv vec x)) (mapv vec x) nil)
    :else (throw (IllegalArgumentException. "cannot make matrix"))))

(defn anxmatrix.core/count [x]
  (if-not
    (matrix? x)
    (cc/count x)
    (* (cc/count x) (count (get x 0)))))

(defn anxmatrix.core/row-count [x] (if-not (matrix? x) (throw (UnsupportedOperationException. "row-count not supported on this type")) (cc/count x)))
(defn anxmatrix.core/column-count [x] (if-not (matrix? x) (throw (UnsupportedOperationException. "column-count not supported on this type")) (cc/count (get x 0))))
(defn anxmatrix.core/square-mat [n e]
  (let [repeater #(repeat n %)]
    (matrix (-> e repeater repeater))))

(defn anxmatrix.core/identity-matrix [n]
  (let [row (conj (repeat (dec n) 0) 1)]
    (vec (for [i (range 1 (inc n))]
           (vec (reduce conj (drop i row) (take i row)))))))

(defn anxmatrix.core/get [m r c] (if-not (matrix? m) (throw (UnsupportedOperationException. "anxmatrix.core/get not supported on this type")) (cc/get (cc/get m r) c)))

(defn anxmatrix.core/set [m r c v] (if-not (matrix? m) (throw (UnsupportedOperationException. "anxmatrix.core/set not supported on this type")) (assoc m r (assoc (cc/get m r) c v))))

(defn anxmatrix.core/add [ma mb] (mapv (fn [x y] (mapv + x y)) ma mb))

(defn anxmatrix.core/det [m] (if-not (and (matrix? m) (= (row-count m) (column-count m))) (throw (UnsupportedOperationException. "anxmatrix.core/det not supported on this type"))) (let [o (range (row-count m))] (reduce + (map (fn [p] (* (Math/pow -1 (cc/get (count-inv p) :inv)) (reduce * (map (fn [r c] (get m r c)) o p)))) (combo/permutations o)))))

(defn sameSize? [& args]
  (every? (fn [x] (== (count (first args)) (count x))) args)
  )

(defn func [op]
  (fn [& args]
    {
     :pre  [(every? vector? args)
            (apply sameSize? args)
            (every? (fn [x] (every? number? x)) args)]
     :post [(vector? %)
            (== (count (first args)) (count %))]
     }
    (apply mapv op args)
    ))

(def v+ (func +))
(def v- (func -))
(def v* (func *))
(def vd (func /))

(defn scalar [& args]
  {
   :pre  [(every? vector? args)
          (apply sameSize? args)
          (every? (fn [x] (every? number? x)) args)]
   :post [(number? %)]
   }
  (apply + (apply v* args)))
(defn v*s [x, & c]
  {:pre  [(vector? x) (every? number? c)]
   :post [(vector? %)
          (== (count x) (count %))]
   }
  (def all (apply * c))
  (mapv #(* % all) x))
(defn m*s [m, & s]
  {:pre  [(vector? m)
          (every? vector? m)
          (every? number? s)]
   :post [(vector? %)
          (== (count m) (count %))
          (== (count (first m)) (count (first %)))]
   }
  (def all (apply * s))
  (mapv #(v*s % all) m))

(defn m*v [m, & v]
  {:pre  [(apply sameSize? v)]
   :post [(vector? %)
          (== (count m) (count %))]
   }
  (def all (apply v* v))
  (mapv #(apply + (v* % all)) m))

(defn transpose [m]
  {:pre  [(vector? m) (every? vector? m)]
   :post [(vector? %)
          (== (count (first m)) (count %))]
   }
  (apply mapv vector m))

(defn vect [& v]
  {:pre  [(every? vector? v)
          (apply sameSize? v)
          (every? (fn [x] (every? number? x)) v)]
   :post [(vector? %)
          (== (count (first v)) (count %))]
   }
  (reduce (fn [a b]
            (let [[x y z] a
                  m [[0 (- z) y]
                     [z 0 (- x)]
                     [(- y) x 0]]]
              (m*v m b))) v))

(defn matrixFabric [op]
  (fn [& m]
    {:pre  [(every? vector? m)
            (apply sameSize? m)]
     :post [(vector? %)
            (== (count (first m)) (count %))]
     }
    (apply mapv op m)
    ))

(defn m*m [& m]
  {:pre  [(every? vector? m) (every? (fn [x] (every? vector? x)) m)]
   :post [(vector? %)
          (== (count (first m)) (count %))]
   }
  (reduce (fn [m1 m2] (mapv (partial m*v (transpose m2)) m1)) m))

(def m+ (matrixFabric v+))
(def m- (matrixFabric v-))
(def m* (matrixFabric v*))
(def md (matrixFabric vd))

(defn simplex? [x]
  (or (and (vector? x) (every? number? x))
      (every? true? (map-indexed
                      #(and (== (count %2) (- (count x) %1)) (simplex? %2))
                      x))
      )
  )

(defn simplexFabric [op1]
  (fn simple [& x]
    {:pre  [(every? simplex? x)
            (apply sameSize? x)]
     :post [(simplex? %)
            (== (count (first x)) (count %))]
     }
    (let
      [op (if (and (vector? (first x)) (every? number? (first x))) op1 #(apply simple %&))]
      (apply mapv op x)
      )
    )
  )

(def x+ (simplexFabric +))
(def x- (simplexFabric -))
(def x* (simplexFabric *))
(def xd (simplexFabric /))
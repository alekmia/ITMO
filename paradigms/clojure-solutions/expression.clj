(defn doMath [op, & mathables]
  (fn [dic]
    (apply op (mapv #(% dic) mathables))
    )
  )

(defn exprConstructor [op]
  (partial doMath op)
  )

(def add
  (exprConstructor +))

(def subtract
  (exprConstructor -))

(def multiply
  (exprConstructor *))

(defn divisionNew [& args]
  (cond
    (= 1 (count args)) (/ 1 (double (nth args 0)))
    :else (reduce (fn [x y] (/ x (double y))) args))
  )

(def divide
  (exprConstructor divisionNew))

(def negate
  (exprConstructor -))

(defn variable [letter]
  (fn [dic]
    (dic letter)))

(defn meanMath [& args]
  (/ (apply + args) (count args)))
(defn varnMath [& args]
  (let [meany (apply meanMath args)]
    (/ (apply + (map #(* (- % meany) (- % meany)) args)) (count args))
    )
  )
(def mean (exprConstructor meanMath))
(def varn (exprConstructor varnMath))

(def constant constantly)

(def myOps {'+      add
            '-      subtract
            '*      multiply
            '/      divide
            'negate negate
            'mean   mean
            'varn   varn})

(defn parseList
  [listik dict conSize varSize]
  (cond
    (number? listik) (conSize listik)
    (symbol? listik) (varSize (str listik))
    :else (apply (dict (first listik)) (map #(parseList % dict conSize varSize) (rest listik)))
    )
  )
(defn parseFunction [string]
  (parseList (read-string string) myOps constant variable))







(definterface Expression
  (evaluate [vars])
  (diff [arg])
  )

(deftype MyConstant [number]
  Expression
  (evaluate [this vars] number)
  (diff [this arg] (MyConstant. 0))
  Object
  (toString [this] (str number))
  )
(defn Constant [number] (MyConstant. number))


(deftype MyVariable [var]
  Expression
  (evaluate [this vars] (vars var))
  (diff [this arg]
    (cond
      (identical? var arg) (Constant 1)
      :else (Constant 0))
    )
  Object
  (toString [this] (str var))
  )
(defn Variable [var] (MyVariable. var))


(deftype MyArsen [args, func, diffFunc, symbol]
  Expression
  (evaluate [this vars] (apply func (map #(.evaluate % vars) args)))
  (diff [this arg] (diffFunc arg))
  Object
  (toString [this] (cond
                     (= 0 (count args)) (str "(" symbol " )")
                     :else (str "(" symbol (reduce str (map #(str " " (.toString %)) args)) ")")))
  )
(defn AddListApply [args] (MyArsen. args + (fn [arg] (AddListApply (map #(.diff % arg) args))) "+"))
(defn Add [& args] (AddListApply args))

(defn SubtractListApply [args] (MyArsen. args - (fn [arg] (SubtractListApply (map #(.diff % arg) args))) "-"))
(defn Subtract [& args] (SubtractListApply args))

(defn Multiply [& args] (MyArsen. args * (fn [arg] (cond
                                                            (empty? args) (Constant 0)
                                                            :else (let [[firstt & restt] args]
                                                                    (Add
                                                                      (apply Multiply (.diff firstt arg) restt)
                                                                      (Multiply (.diff (apply Multiply restt) arg) firstt))
                                                                    ))) "*"))


(defn Divide [& args] (MyArsen. args divisionNew (fn [arg] (let
                                                          [firstt (take (- (count args) 1) args)
                                                           lastt (last args)]
                                                          (cond
                                                            (empty? args) (Constant 0)
                                                            (== 1 (count args)) (.diff (Divide (Constant 1) (first args)) arg)
                                                            (== 2 (count args)) (Divide
                                                                                  (Subtract (Multiply (.diff (first args) arg) (last args))
                                                                                            (Multiply (first args) (.diff (last args) arg)))
                                                                                  (Multiply (last args) (last args))
                                                                                  )
                                                            :else
                                                            (.diff (Divide (first args) (apply Multiply (rest args))) arg)
                                                            )
                                                          )) "/"))

(defn NegateListApply [args] (MyArsen. args - (fn [arg] (NegateListApply (map #(.diff % arg) args))) "negate"))
(defn Negate [& args] (NegateListApply args))



(deftype MyMean [args]
  Expression
  (evaluate [this vars] (/ (apply + (map #(.evaluate % vars) args)) (count args)))
  (diff [this arg]
    (cond
      :else (Divide (.diff (apply Add args) arg) (Constant (count args))))
    )
  Object
  (toString [this] (str "(mean" (reduce str (map #(str " " (.toString %)) args)) ")")))
(defn Mean [& args] (MyMean. args))


(deftype MyVarn [args]
  Expression
  (evaluate [this vars] (-
                          (.evaluate (MyMean. (map #(Multiply % %) args)) vars)
                          (.evaluate (Multiply (apply Mean args) (apply Mean args)) vars)))
  (diff [this arg] (Subtract
                     (.diff (MyMean. (map #(Multiply % %) args)) arg)
                     (.diff (Multiply (apply Mean args) (apply Mean args)) arg)))

  Object
  (toString [this] (str "(varn" (reduce str (map #(str " " (.toString %)) args)) ")")))
(defn Varn [& args] (MyVarn. args))



(defn evaluate [expr vars] (.evaluate expr vars))
(defn diff [expr var] (.diff expr var))
(defn toString [expr] (.toString expr))

(def myObjOps {'+      Add
               '-      Subtract
               '*      Multiply
               '/      Divide
               'negate Negate
               'mean   Mean
               'varn   Varn
               })

(defn parseObject [string]
  (parseList (read-string string) myObjOps Constant Variable))





















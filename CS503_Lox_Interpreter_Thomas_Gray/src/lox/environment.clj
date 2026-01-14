(ns lox.environment)

; empty environment map wrapped in an atom so we can update it without having to return a new map everytime
; made it dynamic for use in interpreter.clj
(def ^:dynamic env
  {:enclosing nil
   :values (atom {})}
)

; creates a new environment for functions
(defn makeEnvironment
  ([enclosing] (makeEnvironment enclosing false))
  ([enclosing isFunctionScope]
   {:enclosing enclosing
    :values    (atom {})
    :isFunctionScope isFunctionScope})
)

(defn define [env name value]
  (let [key (if (map? name) 
              (:lexeme name) 
              ;else
              name)]
    (swap! (:values env) assoc key value) 
    )
)

(defn ancestor [environment distance]
  (loop [env environment
         i 0]
    (if (< i distance)
      (recur (:enclosing env) (inc i))
      ;else return the environment
      env))
)

(defn getVar [env name]
  (let [key (if (map? name) 
              (:lexeme name) 
              ;else
              name)]
    (if (contains? @(:values env) key)
        (get @(:values env) key)
      (if (:enclosing env)
          (getVar (:enclosing env) name)  ;calls getVar on the name
        (throw (ex-info (str "Undefined variable '" key "'") {:name name})))))
)

(defn getAt [environment distance name]
  (let [key (if (map? name) (:lexeme name) name)
        targetEnv (ancestor environment distance)]
    (get @(:values targetEnv) key))
)

(defn assign [env name value]
  (let [key (if (map? name) (:lexeme name) name)]
    (if (contains? @(:values env) key)
      (do
        (swap! (:values env) assoc key value)
        value)
      (if (:enclosing env) 
          (assign (:enclosing env) name value)
        (throw (ex-info (str "Undefined variable '" key "'") {:name name})))))
)

(defn assignAt [environment distance name value]
  (let [key (if (map? name) (:lexeme name) name)
        targetEnv (ancestor environment distance)]
    (swap! (:values targetEnv) assoc key value))
)

(defn makeClock []
  {:arity (fn [_] 0)
   :call  (fn [_ _interpreter _arguments]
            (/ (double (System/currentTimeMillis)) 1000.0))
   :toString (fn [] "<native fn>")}
)
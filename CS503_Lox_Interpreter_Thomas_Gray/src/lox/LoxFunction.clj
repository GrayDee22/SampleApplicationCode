(ns lox.LoxFunction
  (:require [lox.environment :as env]))

(defn makeLoxFunction [declaration closure executeBlockFun & [isInitializer]] ;isInitializer is optional
  (let [params   (:params declaration)
        name     (:lexeme (:name declaration))
        funcMap  (atom nil)
        fm       {:declaration declaration
                  :closure closure
                  :isInitializer (boolean isInitializer)
                  
                  :call (fn [_this _interpreter arguments]
                          (let [environment (env/makeEnvironment (:closure @funcMap) true)]
                            ; bind parameters
                            (loop [i 0]
                              (when (< i (count params))
                                (env/define environment
                                  (:lexeme (nth params i))
                                  (nth arguments i))
                                (recur (inc i))))
                            ; execute body
                            (try 
                              (let [result (executeBlockFun (:body declaration) environment)]
                                ; return this if initializer is true
                                (if (:isInitializer @funcMap)
                                  (env/getAt (:closure @funcMap) 0 "this")
                                  ;else return  result
                                  result))
                              (catch clojure.lang.ExceptionInfo e 
                                (let [value (:value (ex-data e))]
                                      (if (:isInitializer @funcMap)
                                        (env/getAt (:closure @funcMap) 0 "this")
                                        ;else return value
                                        value))))))


                  :arity (fn [_this] (count params))

                  :toString (fn [] (str "<fn " name ">"))
  
                  :bind (fn [this instance] (let [newEnv (env/makeEnvironment (:closure this) true)] 
                                              (env/define newEnv "this" instance) 
                                              (makeLoxFunction (:declaration this)  newEnv  executeBlockFun (:isInitializer this))))
                  }]   
                  (reset! funcMap fm)   
                  fm)
)

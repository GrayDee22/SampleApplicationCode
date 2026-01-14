(ns lox.LoxInstance)

(defn makeLoxInstance [klass]
  (let [fields (atom {})]
    {:klass klass
     :fields fields

     :get (fn [this name] (let [lexeme (:lexeme name)] 
                            (if (contains? @fields lexeme) 
                              (get @fields lexeme) 
                              ;else look for a method and bind it
                              (let [method ((:findMethod klass) klass lexeme)] ; look for a method in the class via the findMethod function in LoxClass 
                                (if method 
                                  ((:bind method) method this)  ; bind the method to this instance, bind function in LoxFunction
                                  ;else
                                  (throw (ex-info (str "Undefined property '" lexeme "'.") {:token name})))))))
     
     :set (fn [_this name value] (let [lexeme (:lexeme name)] 
                                   (swap! fields assoc lexeme value)))
     
     :toString (fn [] (str (:name klass) " instance"))})
)
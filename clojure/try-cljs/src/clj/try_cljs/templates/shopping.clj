(ns try-cljs.templates.shopping
  (:require [net.cgrand.enlive-html :refer [deftemplate add-class set-attr do-> content attr=]]
            [try-cljs.remotes :refer [calculate]]
            [try-cljs.shopping.validators :refer [validate-shopping-form]]))

(defmacro maybe-first-error [kw expr]
  `(if-let [x# (first (~kw ~expr))] (do-> (add-class "help") (content x#)) identity))

(defmacro label-selector [kw] `[[:label (attr= :for (str '~kw))]])

(deftemplate update-shopping-form "shopping.html"
  [q p t d error]

  (label-selector "quantity") (maybe-first-error :quantity error)
  (label-selector "price") (maybe-first-error :price error)
  (label-selector "tax") (maybe-first-error :tax error)
  (label-selector "discount") (maybe-first-error :discount error)

  [:#quantity] (set-attr :value q)
  [:#price] (set-attr :value p)
  [:#tax] (set-attr :value t)
  [:#discount] (set-attr :value d)

  [:#total] (set-attr :value
                      (if error
                        "0.00"
                        (format "%.2f" (double (calculate q p t d))))))

(defn shopping [q p t d]
    (update-shopping-form q p t d (validate-shopping-form q p t d)))

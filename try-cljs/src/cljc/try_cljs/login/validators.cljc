(ns try-cljs.login.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :as pred :refer [present?  matches email-address?]]))

(def ^:const re-password #"^(?=.*\d).{4,8}$")

(defn user-credentials-errors [email password]
  (validate {:email email :password password}
            [:email present? "email can't be empty"]
            [:email email-address? "malformed email"]
            [:password present? "empty password"]
            [:password (matches re-password) "provided password is incorrect"]))

#? (:clj (defn email-domain-errors [email]
           (validate {:email email}
                     [:email pred/valid-email-domain? "mail domain isn't resolvable"])))

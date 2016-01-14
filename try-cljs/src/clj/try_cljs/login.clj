(ns try-cljs.login
  (:require [try-cljs.login.validators :refer [user-credentials-errors]]))


(def ^:const password-re
  #"^(?=.*\d).{4,8}$")

(def ^:const email-re
  #"^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$")

(defn- validate-email [email]
  (re-matches email-re email))

(defn- validate-password [password]
  (re-matches password-re password))

(defn- authenticate-user- [email password]
  (if (some empty? [email password])
    (str "form isn't complete")
    (if (and (validate-email email)
             (validate-password password))
      (str email " and " password " checked. But aren't authenticated"))))
 
(defn authenticate-user [email password]
  (if (boolean (user-credentials-errors email password))
    (str "Please correctly complete the form")
    (str email " and " password " are checked. But user isn't authenticated")))

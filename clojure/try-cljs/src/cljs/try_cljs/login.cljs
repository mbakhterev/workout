(ns try-cljs.login
  (:require [domina.core :refer [by-id by-class value attr
                                 prepend! append! destroy!]]
            [domina.events :refer [listen! prevent-default]]
            [hiccups.runtime :as hiccupstr]
            [try-cljs.login.validators :refer [user-credentials-errors]]
            [shoreleave.remotes.http-rpc :refer [remote-callback]])
  (:require-macros [hiccups.core :refer [html]]
                   [shoreleave.remotes.macros :as shore-macros]))

(. js/console log "try-cljs.login here")

(comment (defn validate-email [email]
  (destroy! (by-class "email"))
  (if (not (re-matches (re-pattern (attr email :pattern))
                       (value email)))
    (do (prepend! (by-id "loginForm")
                  (html [:div.help.email "incorrect email"]))
        false)
    true)))


(defn validate-email-domain [email]
  (. js/console log (str "validating e-mail: " email))
  (remote-callback
    :email-domain-errors [email]
    (fn [r]
      (. js/console log (str "validation response: " r))
      (if r
        (do (prepend! (by-id "loginForm") (html [:div.help.email "wrong email domain"]))
            false)
        true))))

(defn validate-email [email]
  (destroy! (by-class "email"))
  (if-let [err (-> (user-credentials-errors (value email) nil)
                   :email
                   first)]
    (do (prepend! (by-id "loginForm") (html [:div.help.email err]))
        false)
    (validate-email-domain (value email))))

(comment (defn validate-password [password]
  (destroy! (by-class "password"))
  (if (not (re-matches (re-pattern (attr password :pattern))
                       (value password)))
    (do (append! (by-id "loginForm")
                 (html [:div.help.password "incorrect password"]))
        false)
    true)))

(defn validate-password [password]
  (destroy! (by-class "password"))
  (if-let [err (-> (user-credentials-errors nil (value password))
                   :password
                   first)]
    (do (prepend! (by-id "loginForm") (html [:div.help.password err]))
        false)
    true))

(comment (defn validate-form [e]
  (let [[email password :as fields] (map by-id ["email" "password"])]
    (if (every? (comp empty? value) fields)
      (do (destroy! (by-class "help"))
          (prevent-default e)
          (append! (by-id "loginForm")
                   (html [:div.help "please, complete the form"])))
      (if (and (validate-email email)
               (validate-password password))
        true
        (prevent-default e))))))

(defn validate-form [e email password]
  (let [{e-err :email p-err :password} (user-credentials-errors (value email)
                                                                (value password))]
    (if (or e-err p-err)
      (do (destroy! (by-class "help"))
          (append! (by-id "loginForm") (html [:div.help "please, correct input"]))
          (prevent-default e)
          false)
      true)))

(defn ^:export init []
  (. js/console log "login init")
  (if (and js/document
           (aget js/document "getElementById"))
    (let [[submit email password] (map by-id ["submit" "email" "password"])]
      (listen! submit
               :click
               (fn [e] (validate-form e email password)))
      (listen! email
               :blur
               (fn [e] (validate-email email)))
      (listen! password
               :blur
               (fn [e] (validate-password password))))))

;     (let [login-form (.getElementById js/document "loginForm")]
;       (set! (.-onsubmit login-form) validate-form))))
; 
; (set! (.-onload js/window) init)

; (defn get-element [n] (. js/document getElementById n))

; (defn validate-form [e]
;   (. js/console log "validating")
;   (let [el-something? (comp not empty? value by-id)]
;     (if (and (el-something? "email")
;              (el-something? "password"))
;       true
;       (do
;           (prevent-default e)
;           (js/alert "Please, complete the form")
;           false))))

; (def ^:const password-re
;   #"^(?=.*\d).{4,8}$")
; 
; (def ^:const email-re
;   #"^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$")
 

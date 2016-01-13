(ns try-cljs.login
  (:require [domina.core :refer [by-id by-class value
                                 prepend! append! destroy!]]
            [domina.events :refer [listen! prevent-default]]
            [hiccups.runtime])
  (:require-macros [hiccups.core :refer [html]]))

(. js/console log "try-cljs.login here")

(def ^:const password-re
  #"^(?=.*\d).{4,8}$")

(def ^:const email-re
  #"^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$")

(defn validate-form [e]
  (let [[email password :as fields] (map by-id ["email" "password"])]
    (if (every? (comp empty? value) fields)
      (do (destroy! (by-class "help"))
          (prevent-default e)
          (append! (by-id "loginForm")
                   (html [:div.help "please, complete the form"])))
      (if (and (validate-email email)
               (validate-password password))
        true
        (prevent-default e)))))

(defn validate-email [email]
  (destroy! (by-class "email"))
  (if (not (re-matches email-re (value email)))
    (do (prepend! (by-id "loginForm")
                  (html [:div.help.email "incorrect email"]))
        false)
    true))

(defn validate-password [password]
  (destroy! (by-class "password"))
  (if (not (re-matches password-re (value password)))
    (do (append! (by-id "loginForm")
                 (html [:div.help.password "incorrect password"]))
        false)
    true))

(defn ^:export init []
  (. js/console log "login init")
  (if (and js/document
           (aget js/document "getElementById"))
    (let [[submit email password] (map by-id ["submit" "email" "password"])]
      (listen! submit
               :click
               validate-form)
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

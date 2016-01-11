(ns try-cljs.login
  (:require [domina.core :refer [by-id value set-value!]]))

(defn get-element [n] (. js/document getElementById n))

(defn validate-form []
  (let [el-len (comp count value by-id)]
    (if (and (< 0 (el-len "email"))
             (< 0 (el-len "password")))
      true
      (do (js/alert "Please, complete the form")
          false))))

(defn ^:export init []
  (if (and js/document (.-getElementById js/document))
    (let [login-form (.getElementById js/document "loginForm")]
      (set! (.-onsubmit login-form) validate-form))))

; (set! (.-onload js/window) init)

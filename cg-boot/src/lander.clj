(ns lander (:require [render :as r]
                     [records :refer :all]))

; Вспомогательные функции
(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

; Тестовые данные
(defn ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                              800 1800 1000 2500 1200 2100 1500 2400
                                              2000 1000 2200 500 2500 100 2900 800
                                              3000 500 3200 1000 3500 2000 3800 800
                                              4000 200 5000 200 5500 1500 6999 2800]
                                    :lander [500 2700 100 0 800 -90 0]}])

(r/update-scene :surface (list (make-section (->Point 0 200) (->Point 300 400))
                               (make-section (->Point 300 400) (->Point 600 700))
                               (make-section (->Point 0 0) (->Point 6999 2999))))

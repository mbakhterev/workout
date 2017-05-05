(defn- hover-align-control [^Stage {section :section :as stage} ^Lander lander ^long angle ^long power]
  (loop [l lander t 0.0]
    (cond (not (over-line? l section)) (assoc l :state :ko)

          (not (in-range? l section)) (if (constraint l stage) (assoc l :state :out :dt t) (assoc l :state :ko))
          
          (and (= angle (:angle l)) (= power (:power l))) (assoc l :dt t)

          :else (recur (move angle power 1.0 l) (+ 1.0 t)))))


(define control-to
  (the (lambda (Control Control) Control)
       (lambda (f t)
         (let ((angle-max-delta 15)
               (power-max-delta 1)
               (tune-value (the (lambda (long long long) long)
                                (lambda (current goal max-delta)
                                  (let ((delta (- goal current)))
                                    (cond (= 0 delta) goal
                                          (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
                                          (> 0 delta) (if (> delta (- max-delta)) goal (- current max-delta))))))))
           (Control (tune-value (:angle f) (:angle t) angle-max-delta)
                    (tune-value (:power f) (:power t) power-max-delta))))))


(это ((цикл-приближения
       (тип (лямбда (Модуль Секция (Угол (Целое 32)) (Энергия (Целое 32)))
                    (кортеж (+ :out :ko :ok) Модуль (Плавающее 32)))

            (лямбда (исходный-модуль секция угол энергия)
                    (это ((ax (:ax скц))
                          (bx (:bx скц))
                          (шаг (лямбда (м-пред) (это ((м (движение угл энергия 1.0 м-пред)))
                                                     (условно ((не (<= ax (:x м) bx)) (кортеж :out м-пред))
                                                              ((не (над-секцией? м секция)) (кортеж :ko м-пред))
                                                              (иначе (шаг м)))))))
                          (шаг модуль)))))))

(это ((цикл-приближения
       (тип (лямбда (Модуль Секция (Угол (Целое 32)) (Энергия (Целое 32))) (кортеж (+ :out :ko :ok) Модуль (Плавающее 32)))
            (лямбда (исходный-модуль секция угол энергия)
                    (это ((ax (:ax секция))
                          (bx (:bx секция))
                          (шаг (лямбда (м-пред) (это ((м (движение угол энергия 1.0 м-пред)))
                                                     (условно ((не (<= ax (:x м) bx)) (кортеж :out м-пред))
                                                              ((не (над-секцией? м секция)) (кортеж :ko м-пред))
                                                              (иначе (шаг м)))))))
                         (шаг модуль)))))))

(это цикл-приближения
     (тип (лямбда Модуль Секция (Угол (Целое 32)) (Энергия (Целое 32)) (кортеж (+ :out :ko :ok) Модуль (Плавающее 32)))
          (лямбда исходный-модуль секция угол энергия
                  (это ax (:ax скц)
                       bx (:bx скц)
                       шаг (лямбда м-пред (это м (движение угл энергия 1.0 м-пред)
                                               (условно (не (<= ax (:x м) bx)) (кортеж :out м-пред)
                                                        (не (над-секцией? м секция)) (кортеж :ko м-пред)
                                                        иначе (шаг м))))
                       (шаг модуль)))))

Есть ещё, кстати, синтаксис для монад. И тут вопрос: как оно зайдёт?

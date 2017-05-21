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

(это цикл-приближения
     (тип (лямбда Модуль Секция (Угол (Целое 32)) (Энергия (Целое 32)) (кортеж (+ :out :ko :ok) Модуль (Плавающее 32)))
          (лямбда исходный-модуль секция угол энергия
                  (это ax  (:ax скц)
                       bx  (:bx скц)
                       шаг (лямбда м-пред (это м (движение угл энергия 1.0 м-пред)
                                               (условно (не (<= ax (:x м) bx))       (кортеж :out м-пред)
                                                        (не (над-секцией? м секция)) (кортеж :ko м-пред)
                                                        иначе                        (шаг м))))
                       (шаг модуль)))))

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

На самом деле, и то, и другое вполне приемлемо. Например:

(let approximate-move
     (lambda ((type control Control) (type trace (List Lander)))
             (let ((l (move control 1.0 (last trace))))
                  (append trace (assoc l (x (round (l x)))
                                         (y (round (l y)))
                                         (vx (round (l vx)))
                                         (vy (round (l vy))))))))

Но, всё же, громоздко. И если мы хотим общих принципов, то разумнее действовать так:

(let approximate-move
     (lambda (the Control control) (the (List Lander) lander)
             (let l (move control 1.0 (last trace))
                  (append trace (assoc l :x  (round (l :x))
                                         :y  (round (l :y))
                                         :vx (round (l :vx))
                                         :vy (round (l :vy)))))))

(let ((approximate-move
        (lambda ((the Control control) (the List lander))
          (let ((l (move control 1.0 (last trace))))
            (append trace (assoc l (:x (round (l :x)))))
            )
          )
        )))

Больше примеров, больше хардкора.

(def gauss-random (sigma (o mu 0))
     "gausian distributed random with width sigma around mu"
     (withs (u (rand)
               v (* 1.7156 (- (rand) 0.5))
               x (- u 0.449871)
               y (+ abs.v 0.386595)
               q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
            (while (and (> q 0.27597)
                        (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                   (= u (rand)
                      v (* 1.7156 (- (rand) 0.5))
                      x (- u 0.449871)
                      y (+ abs.v 0.386595)
                      q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
            (+ mu (/ (* sigma v) u))))

(define (gauss-random sigma mu)
        (loop u (rand)
              v (* 1.7156 (- (rand) 0.5))
              x (- u 0.449871)
              y (+ abs.v 0.386595)
              q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))
              (if (and (> q 0.27597)
                       (or (> q 0.27846) (> (* v v) (* -4 log.u u u))))
                  (recur (rand)
                         (* 1.7156 (- (rand) 0.5))
                         (- u 0.449871)
                         (+ abs.v 0.386595)
                         (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
                  (+ mu (/ (* sigma v) u)))))

(define (gauss-random sigma mu)
  (let next (u (rand)
             v (* 1.7156 (- (rand) 0.5))
             x (- u 0.449871)
             y (+ (abs v) 0.386595)
             q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
    (if (and (> q 0.27597)
             (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
      (next (rand)
            (* 1.7156 (- (rand) 0.5))
            (- u 0.449871)
            (+ (abs v) 0.386595)
            (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
      (+ mu (/ (* sigma v) u)))))

(define (access-allowed? command-line ssh-line)
  ; Очевидные вспомогательные процедуры. FIXME: вероятно, можно написать лучше
  (define (valid-user? usr) (find (lambda (u) (equal? usr u)) users))
  (define (user-command? cmd) (find (lambda (c) (equal? cmd c)) user-git-commands))
  (define (group-command? cmd) (find (lambda (c) (equal? cmd c)) group-git-commands))
  (define (user-from-path path) (car (string-split path #\/)))

  ; Извлечение из ssh-команды первого компонента пути, который, технически,
  ; задаёт пользователя

  (define (user-from-line cmd-line)
    ; Для разных команд способ извлечения пользователя разный. Поэтому полезно
    ; иметь различающие их процедуры
    
    ; Процедура для git
    (define (git-case cmd)
      (find (lambda (c) (equal? c (car cmd)))
            (append user-git-commands group-git-commands)))

    (cond
      ((git-case cmd-line) (user-from-path (cadr cmd-line)))))

  (let ((path-user (user-from-line ssh-line))
        (cmd-user (user command-line))
        (cmd (command ssh-line)))
    (and (valid-user? path-user)
         (or (group-command? cmd)
             (and (user-command? cmd) (equal? cmd-user path-user))))))

(define (access-allowed? (the (List String) command-line) (the (List String) ssh-line))
  (let (valid-user?    (lambda (usr) (find (partial equal? usr) users))
        user-command?  (lambda (cmd) (find (partial equal? cmd) user-git-commands))
        group-command? (lambda (cmd) (find (partial equal? cmd) group-git-commands))
        user-from-path (lambda (pth) (first (string-split pth \/)))

        user-from-line (lambda (cmd-line)
                         (let (git-case (lambda (cmd) (find (partial equal? (head cmd))
                                                                 (append user-git-commands
                                                                         group-git-commands))))
                           (cond (git-case cmd-line) (user-from-path (second cmd-line)))))
        
        path-user      )))

(define gauss-random
  (one-of
    (lambda (sigma) (gauss-random sigma 0.0))
    (lambda (gauss-random sigma mu)
      (let loop ((u (rand))
                 (v (* 1.7156 (- (rand) 0.5)))
                 (x (- u 0.449871))
                 (y (+ (abs v) 0.386595))
                 (q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x))))))
        (if (and (> q 0.27597)
                 (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
          (loop (rand)
                (* 1.7156 (- (rand) 0.5))
                (- u 0.449871)
                (+ (abs v) 0.386595)
                (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
          (+ mu (/ (* sigma v) u)))))))

(define gauss-random
  (one-of
    (lambda (sigma) (gauss-random sigma 0.0))
    (lambda (gauss-random sigma mu)
      (let loop (u (rand)
                 v (* 1.7156 (- (rand) 0.5))
                 x (- u 0.449871)
                 y (+ (abs v) 0.386595)
                 q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
        (if (and (> q 0.27597)
                 (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
          (loop (rand)
                (* 1.7156 (- (rand) 0.5))
                (- u 0.449871)
                (+ (abs v) 0.386595)
                (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
          (+ mu (/ (* sigma v) u)))))))



(define gauss-random
  (one-of
    (λ (sigma) (gauss-random sigma 0.0))
    (λ (gauss-random sigma mu)
      (let loop (u (rand)
                 v (* 1.7156 (- (rand) 0.5))
                 x (- u 0.449871)
                 y (+ (abs v) 0.386595)
                 q (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
        (if (and (> q 0.27597)
                 (or (> q 0.27846) (> (* v v) (* -4 (log u) u u))))
          (loop (rand)
                (* 1.7156 (- (rand) 0.5))
                (- u 0.449871)
                (+ (abs v) 0.386595)
                (+ (* x x) (* y (- (* 0.196 y) (* 0.25472 x)))))
          (+ mu (/ (* sigma v) u)))))))

(define (access-allowed? (the (List String) command-line) (the (List String) ssh-line))
  (let ((valid-user? (lambda (usr) (find (partial equal? usr) users)))
        (user-command? (lambda (cmd) (find (partial equal? cmd) user-git-commands)))
        (group-command? (lambda (cmd) (find (partial equal? cmd) group-git-commands)))
        (user-from-path (lambda (pth) (first (string-split pth \/))))

        (user-from-line
          (lambda (cmd-line)
            (let ((git-case (lambda (cmd) (find (partial equal? (head cmd))
                                                (append user-git-commands
                                                        group-git-commands)))))
              (cond ((git-case cmd-line) (user-from-path (second cmd-line)))))))
        
        (path-user (user-from-line ssh-line))
        (cmd-user (user command-line))
        (cmd (command ssh-line)))
    (and (valid-user? path-user)
         (or (group-command? cmd)
             (and (user-command? cmd) (equal? cmd-user path-user))))))


(define (access-allowed? (the (List String) command-line) (the (List String) ssh-line))
  (let (valid-user?    (lambda (usr) (find (partial equal? usr) users))
        user-command?  (lambda (cmd) (find (partial equal? cmd) user-git-commands))
        group-command? (lambda (cmd) (find (partial equal? cmd) group-git-commands))
        user-from-path (lambda (pth) (first (string-split pth \/)))

        user-from-line (lambda (cmd-line)
                         (let ((git-case (lambda (cmd) (find (partial equal? (head cmd))
                                                             (append user-git-commands
                                                                     group-git-commands)))))
                           (cond ((git-case cmd-line) (user-from-path (second cmd-line))))))
        
        path-user      (user-from-line ssh-line)
        cmd-user       (user command-line)
        cmd            (command ssh-line))
    (and (valid-user? path-user)
         (or (group-command? cmd)
             (and (user-command? cmd) (equal? cmd-user path-user))))))




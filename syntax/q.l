(def side-length 200)
(def spacing 5)

(defn draw-point
  [x y z noise-factor]
  (push-matrix)
  (translate x y z)
  (let [grey (* noise-factor 255)]
    (fill grey 10)
    (box spacing spacing spacing)
    (pop-matrix)))

(defn draw []
  (background 0)

  (let [fc          (frame-count)
        x-start     (state :x-start)
        y-start     (state :y-start)
        z-start     (state :z-start)
        rotate-val  (* fc 0.1)
        noise-shift (* fc 0.01)]

    (translate 150 20 -150)
    (rotate-z rotate-val)
    (rotate-y rotate-val)
    (doseq [[x-idx z] (indexed-range-incl 0 side-length spacing)
            [y-idx y] (indexed-range-incl 0 side-length spacing)
            [z-idx x] (indexed-range-incl 0 side-length spacing)]
      (let [x-noise (mul-add x-idx 0.1 (+ noise-shift x-start))
            y-noise (mul-add y-idx 0.1 (+ noise-shift y-start))
            z-noise (mul-add z-idx 0.1 (+ noise-shift z-start))]
        (draw-point x y z (noise x-noise y-noise z-noise))))))

(defn setup []
  (background 0)
  (no-stroke)
  (set-state! :x-start (random 10)
              :y-start (random 10)
              :z-start (random 10)))

(defsketch gen-art-28
  :title "A Cube of 3D Noise"
  :setup setup
  :draw draw
  :size [500 300]
  :renderer :p3d)

(defn -main [& args])

Так. У нас потерялись данные. Это плохо. Пробуем восстановить. Был какой-то
такой вариант:

(def draw-point
  (x y z noise-factor
    (do (push-matrix)
        (translate x y z)
        (let grey (* noise-factor 255)
          (fill grey 10)
          (box spacing spacing spacing)
          (pop-matrix)))))

Но с таким форматом возникает вопрос: а что делать, если нужно просто вернуть x?

  (def draw-point (x y z noise-factor x))

В конструкции let такие же проблемы:

  (let z (+ x y) z)

ПРОБЛЕМА 1. Видимо, это не такая уж и большая проблема, в том смысле, что её в
любом случае нужно будет решать, потому что let у нас последовательный, и надо
как-то правильно отрабатывать:

  (let x 1
       y 2
       z (+ x y) z)

В принципе, вроде как, это решаемо. По крайней мере, в рекурсивном стиле:

  (let-one x 1 (let-one y 2 (let-one z (+ x y) z)))

Но тогда придётся делать (do ...) со всеми вытекающими дополнительными
скобками. Ну, или не надо? В принципе, может быть, и не надо. Можно запнуться за
первое определённое выражение. Ну, думать надо. Ладно посмотрим:

(def side-length 200)
(def spacing 5)

(def draw-point
  (x y z noise-factor
    (push-matrix)
    (translate x y z)
    (let grey (* noise-factor 255)
      (fell grey 10)
      (box spacing spacing spacing)
      (pop-matrix))))

(def draw
  ((background 0) ..))

Нет. Это как-то совсем неоднородно. Но, допустим, тогда scheme-стиль

(def (draw)
  (background 0)

  (let fc          (frame-count)
       x-start     (state :x-start)
       y-start     (state :y-start)
       z-start     (state :z-start)
       rotate-val  (* fc 0.1)
       noise-shift (* fc 0.01)

    (translate 150 20 -150)
    (rotate-z rotate-val)
    (rotate-y rotate-val)

    (doseq (x-idx z) (indexed-range-incl 0 side-length spacing)
           (y-idx y) (indexed-range-incl 0 side-length spacing)
           (z-idx x) (indexed-range-incl 0 side-length spacing)
      (let x-noise (mul-add x-idx 0.1 (+ noise-shift x-start))
           y-noise (mul-add y-idx 0.1 (+ noise-shift y-start))
           z-noise (mul-add z-idx 0.1 (+ noise-shift z-start))
        (draw-point x y z (noise x-noise y-noise z-noise))))))

Тут, конечно же, ещё одна проблема.

ПРОБЛЕМА 2. Она связана с форматированием. Когда у нас есть дополнительная
группировка в виде (let (...) ...), то за скобочку можно зацепиться в
форматировании. А как зацепиться здесь?

  (let x 1
       y 2
       z (+ x y)
    (rotate-z z)
    (rotate-y y))

Вариант с дополнительной скобкой. Зацепиться просто

  (let (x 1
        y 2
        z (+ x y))
    (rotate-z z)
    (rotate-y y))
    
ПРОБЛЕМА 3. Всё же это проблема. В принципе, мы можем вычислить то, что
(rotate-z z) не содержит свободных переменных. Ну, или дадим им какое-нибудь
хитрое название. Но это довольно замученный метод. А если «z» в результате
правок поменяется? Всё переформатировать?

РЕШЕНИЕ 3.1. Для самоудовлетворения: всё равно анализ на свободные и связанные
выражения надо будет делать внутри let и для переменных и для разбора параметров
и для прочего, прочего, прочего.

РЕШЕНИЕ 3.2. Имеет смысл выделить область binding-а.
(def side-length 200)
(def spacing 5)

(def (draw-point x y z noise-factor)
  (push-matrix)
  (translate x y z)
  (let (grey (* noise-factor 255))
    (fill grey 10)
    (box spacing spacing spacing)
    (pop-matrix)))

(def (draw)
  (background 0)

  (let (fc          (frame-count)
        start-pos   ((juxt :x-start :y-start :z-start) state)
        rotate-val  (* fc 0.1)
        noise-shift (* fc 0.01)
        R           (index-range-incl 0 side-length spacing))

    
    (translate 150 20 -150)
    (rotate-z rotate-val)
    (rotate-y rotate-val)

    (doseq ((x-idx z) R
            (y-idx y) R
            (z-idx x) R)
      (let ((x-noise
             y-noise 
             z-noise) (map (fn (i s) (mul-add i 0.1 (+ noise-shift s)))
                           (vec x-idx y-idx z-idx)
                           (start-pos)))
        (draw-point x y z (noise x-noise y-noise z-noise))))))

(def (setup)
  (background 0)
  (no-stroke)
  (set-state! :x-start (random 10)
              :y-start (random 10)
              :z-start (random 10)))

(def-sketch gen-art-28
  :title "A Cube of 3D Noise"
  :setup setup
  :draw draw
  :size (vec 500 300)
  :renderer :p3d)

(def (main & args))

Как-то пока так. Вывод из этого всего: надо менять нотацию для математики.
Потому что язык - это язык, а математический рендеринг - это математический
рендеринг. Так?

Пока так.

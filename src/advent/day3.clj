(ns advent.day3)

(defn advance-pos
  [x y dir]
  (case dir
    :right [(inc x) y]
    :up    [x (inc y)]
    :left  [(dec x) y]
    :down  [x (dec y)]))

(defn calculate-step
  [x y current-dir min-x min-y max-x max-y]
  (let [[x y] (advance-pos x y current-dir)]
    ;; Figure out if we need to switch directions and update min/max values
    (cond (and (= current-dir :right) (> x max-x)) [x y :up    min-x min-y x     max-y]
          (and (= current-dir :up)    (> y max-y)) [x y :left  min-x min-y max-x y]
          (and (= current-dir :left)  (< x min-x)) [x y :down  x     min-y max-x max-y]
          (and (= current-dir :down)  (< y min-y)) [x y :right min-x y     max-x max-y]
          :else [x y current-dir min-x min-y max-x max-y])))

(calculate-step 0 0 :right 0 0 0 0)
(calculate-step 1 0 :up 0 0 1 0 )

(defn calculate-grid-pos
  [target-val]
  (loop [val 1
         x 0
         y 0
         dir :right
         min-x 0
         min-y 0
         max-x 0
         max-y 0]
    (if (= val target-val)
      [x y]
      (let [[x y dir min-x min-y max-x max-y] (calculate-step x y dir min-x min-y max-x max-y)]
        (recur (inc val) x y dir min-x min-y max-x max-y)))))

(calculate-grid-pos 1)
(calculate-grid-pos 2)
(calculate-grid-pos 3)
(calculate-grid-pos 4)
(calculate-grid-pos 5)
(calculate-grid-pos 23)
(calculate-grid-pos 17)
(calculate-grid-pos 81)

(defn carry-distance
  [value]
  (let [[x y] (calculate-grid-pos value)]
    (+ (Math/abs x) (Math/abs y))))

(carry-distance 1)
(carry-distance 12)
(carry-distance 23)
(carry-distance 1024)
(carry-distance 289326)

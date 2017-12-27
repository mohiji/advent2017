(ns advent.day2
  (:require [clojure.string :as str]))

(def sample-spreadsheet
  [[5 1 9 5]
   [7 5 3]
   [2 4 6 8]])

(defn score-row [row]
  (let [minimum (apply min row)
        maximum (apply max row)]
    (- maximum minimum)))

(defn checksum [spreadsheet]
  (apply + (for [row spreadsheet]
             (score-row row))))

(checksum sample-spreadsheet)


(def day2-input
  "626	2424	2593	139	2136	163	1689	367	2235	125	2365	924	135	2583	1425	2502
  183	149	3794	5221	5520	162	5430	4395	2466	1888	3999	3595	195	181	6188	4863
  163	195	512	309	102	175	343	134	401	372	368	321	350	354	183	490
  2441	228	250	2710	200	1166	231	2772	1473	2898	2528	2719	1736	249	1796	903
  3999	820	3277	3322	2997	1219	1014	170	179	2413	183	3759	3585	2136	3700	188
  132	108	262	203	228	104	205	126	69	208	235	311	313	258	110	117
  963	1112	1106	50	186	45	154	60	1288	1150	986	232	872	433	48	319
  111	1459	98	1624	2234	2528	93	1182	97	583	2813	3139	1792	1512	1326	3227
  371	374	459	83	407	460	59	40	42	90	74	163	494	250	488	444
  1405	2497	2079	2350	747	1792	2412	2615	89	2332	1363	102	81	2346	122	1356
  1496	2782	2257	2258	961	214	219	2998	400	230	2676	3003	2955	254	2250	2707
  694	669	951	455	2752	216	1576	3336	251	236	222	2967	3131	3456	1586	1509
  170	2453	1707	2017	2230	157	2798	225	1891	945	943	2746	186	206	2678	2156
  3632	3786	125	2650	1765	1129	3675	3445	1812	3206	99	105	1922	112	1136	3242
  6070	6670	1885	1994	178	230	5857	241	253	5972	7219	252	806	6116	4425	3944
  2257	155	734	228	204	2180	175	2277	180	2275	2239	2331	2278	1763	112	2054")

(def day2-lines (str/split-lines day2-input))

(defn parse-line [line]
  (into [] (map #(Integer/parseInt %) (-> (str/trim line)
                                          (str/split #"\t")))))

(parse-line (first day2-lines))

(def day2-spreadsheet
  (into [] (map parse-line day2-lines)))

(checksum day2-spreadsheet)

(def sample2
  [[5 9 2 8]
   [9 4 7 3]
   [3 8 6 5]])

;; Start by sorting each line, so I can be sure I'm comparing in ascending order.
;; For each number1 in the line
;; For each number2 remaining in the line
;; If number2 % number1 == 0, return number2 / number1, else return 0

(defn find-even-divisor
  [val potentials]
  (loop [p potentials]
    (if-let [d (first p)]
      (if (= 0 (mod val d))
        d
        (recur (rest p)))
      nil)))

(find-even-divisor 9 [8 5 2])
(find-even-divisor 8 [5 2])

(defn score-line2 [line]
  (loop [vals (sort > line)]
    (if-let [v (first vals)]
      (if-let [d (find-even-divisor v (rest vals))]
        (/ v d)
        (recur (rest vals)))
      (throw (ex-info "Ran through the list without finding a value." {:line line})))))

(score-line2 [2 8 5 9])
(score-line2 [9 4 7 3])
(score-line2 [3 8 6 5])
(apply + (map score-line2 day2-spreadsheet))

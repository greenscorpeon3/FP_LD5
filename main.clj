(require '[clojure.string :as s])

(defn cipher
  [totalRailNum, railPos, letterPos]
  (def offset (- (* totalRailNum 2) 2))
  (def evenOffset (* (- (- totalRailNum 1) railPos) 2))
  (def oddOffset (* railPos 2))

  (cond
    (= letterPos 0 ) railPos
    (or (= railPos 0) (= railPos (- totalRailNum 1))) (+ (cipher totalRailNum railPos (- letterPos 1)) offset)
    (= (mod (+ letterPos 1) 2) 0) (+ (cipher totalRailNum railPos (- letterPos 1)) evenOffset)
    :else (+ (cipher totalRailNum railPos (- letterPos 1)) oddOffset)
    )
)

(defn replacement
  [message]
  (s/replace message #" " "_")
)

(defn charat [s i] (subs s i (+ i 1)))

(defn encryptIndeces
  [key maxLen]
  (filter  (fn [x] (< x maxLen)) (for [x (range key) 
      y (range maxLen)]
  (cipher key x y))))  

(defn encrypt
  [key txt]
  (def text (replacement txt))
  (map  (fn [x] (charat text x)) (encryptIndeces key (count text))))
  

(defn decrypt  [key txt]
  (def text (replacement txt))
  (def eIndeces (encryptIndeces key (count text)))
  (def r (range (count text)))
  (map (fn [l] (charat text (second l))) (sort-by first (map vector eIndeces r))))


(def original "I am the Senate")
(println (str "Original: " original))
(def encrypted (apply str (encrypt 4 original)))
(println (str "encrypted: " encrypted))
(println (str "decrypted: " (apply str (decrypt 4 encrypted))))
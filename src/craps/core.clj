(ns craps.core)

(defn- throw-die
  "Throw a regular, six-sided die."
  []
  (inc (rand-int 6)))

(defn- throw-dice
  "Throw N dice."
  [n]
  (repeatedly n throw-die))

(defn- total-pips
  [dice]
  (apply + dice))

(defn- snake-eyes?
  "Is a throw of 2 dice snake-eyes (two ones)?"
  [[die-1 die-2]]
  (= 2
     (+ die-1 die-2)))

(defn- boxcars?
  "Is a throw of 2 dice boxcars (two sixes)?"
  [[die-1 die-2]]
  (= 12
     (+ die-1 die-2)))

(defn- instant-win?
  "Is a throw of two dice an instant win?"
  [dice]
  (let [pips (total-pips dice)]
    (or (= pips 7)
        (= pips 11))))

(defn- instant-lose?
  "Is a throw of two dice an instant lose?"
  [dice]
  (let [pips (total-pips dice)]
    (or (= pips 2)
        (= pips 3)
        (= pips 12))))

(defn- instant-point-lose?
  "Is a throw of two dice an instant lose while trying for Point?"
  [dice]
  (= (total-pips dice) 7))

(defn- say-throw
  "A human-friendly representation of a throw of two dice."
  [dice]
  (cond
    (snake-eyes? dice) "snake-eyes"
    (boxcars? dice) "boxcars"
    :else (total-pips dice)))

(defn- describe-throw
  "A description of a throw of two dice; either a win or a loss."
  [[die-1 die-2 :as dice] win-or-lose]
  (str "throw " die-1 " and " die-2 " -- " (say-throw dice) " -- you " win-or-lose))

(defn- describe-throw-again
  "A description of a throw of two dice; again."
  [[die-1 die-2 :as dice]]
  (str "throw " die-1 " and " die-2 " -- " (say-throw dice) " -- throw again"))

(defn- describe-point
  "A description of a throw of two dice; a point."
  [[die-1 die-2 :as dice]]
  (str "throw " die-1 " and " die-2 " -- your point is " (say-throw dice)))

(defn craps
  "Simulate the first throw in a game of Craps."
  []
  (let [dice (throw-dice 2)]
    (cond
      (instant-win? dice) (describe-throw dice "win")
      (instant-lose? dice) (describe-throw dice "lose")
      :else (describe-point dice))))

(defn try-for-point
  "Simulate trying for Point in a game of Craps."
  [point]
  (let [dice (throw-dice 2)]
    (cond
      (= (total-pips dice) point) (describe-throw dice "win")
      (instant-point-lose? dice) (describe-throw dice "lose")
      :else (describe-throw-again dice))))
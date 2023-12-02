(ns day02
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader StringReader)))

(defn set-text->set
  "
  Given set text like:
    3 blue, 4 red
  Returns a data structure like:
    {:blue 3 :red 4}
  "
  [input]
  (let [blue (->> input (re-find #"(\d+) blue") second)
        red (->> input (re-find #"(\d+) red") second)
        green (->> input (re-find #"(\d+) green") second)]
    {:blue  (if blue (Integer/parseInt blue) 0)
     :red   (if red (Integer/parseInt red) 0)
     :green (if green (Integer/parseInt green) 0)}))

(defn game-text->game
  "
  Given game text like:
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  returns a data structure like:
    {:id 1 :sets [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]}
  "
  [input]
  (let [id (->> input (re-find #"^Game (\d+)") second Integer/parseInt)
        sets-part (-> input (str/split #":") second)
        sets-strings (-> sets-part (str/split #";"))
        sets (->> sets-strings (map set-text->set))]
    {:id id :sets sets}))

(defn set-possible?
  "Given a set data structure, returns whether the set is possible"
  [set]
  (and
    (<= (:blue set) 14)
    (<= (:red set) 12)
    (<= (:green set) 13)))

(defn game-possible?
  "Given a game data structure, returns whether that game is possible"
  [game]
  (every? set-possible? (:sets game)))

(defn sum-ids-possible-games
  "Returns the sum of the IDs of possible color block games"
  [input]
  (let [lines (-> input StringReader. BufferedReader. line-seq)]
    (->> lines
         (map game-text->game)
         (filter game-possible?)
         (map :id)
         (reduce +))))

(defn min-color
  "Returns the min possible of the given color for the given game"
  [game color]
  (->> game
       :sets
       (map color)
       (apply max)))

(defn min-cubes
  "Returns the min possible cube set for the given game to be possible"
  [game]
  {:blue (min-color game :blue)
   :red (min-color game :red)
   :green (min-color game :green)})

(defn power
  "Returns the power of the given cube set"
  [set]
  (* (:red set) (:blue set) (:green set)))

(defn sum-powers-min-possible-cubes
  "Returns the sum of the powers of the min possible cube sets for the games to have been possible"
  [input]
  (let [lines (-> input StringReader. BufferedReader. line-seq)]
    (->> lines
         (map game-text->game)
         (map min-cubes)
         (map power)
         (reduce +))))

(def sample "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
(def input "Game 1: 3 blue, 2 green, 6 red; 17 green, 4 red, 8 blue; 2 red, 1 green, 10 blue; 1 blue, 5 green\nGame 2: 9 red, 2 green; 5 red, 1 blue, 6 green; 3 green, 13 red, 1 blue; 3 red, 6 green; 1 blue, 14 red, 6 green\nGame 3: 6 red, 3 blue, 8 green; 6 blue, 12 green, 15 red; 3 blue, 18 green, 4 red\nGame 4: 1 blue, 4 red; 2 blue, 6 red; 13 blue; 11 blue, 1 green, 8 red; 10 blue, 3 green, 2 red; 3 green, 7 blue\nGame 5: 2 red, 1 blue, 8 green; 2 blue, 7 green, 3 red; 1 blue, 7 green, 4 red; 2 blue, 1 green, 1 red; 13 green, 1 blue\nGame 6: 7 green, 1 red, 3 blue; 1 red, 4 blue; 6 green, 6 blue; 8 green, 1 red; 6 green, 1 red, 5 blue\nGame 7: 10 blue, 1 green; 5 red, 8 blue, 3 green; 11 blue, 5 red, 8 green; 2 blue, 8 red, 5 green; 7 blue, 9 green; 6 blue, 2 green, 7 red\nGame 8: 15 green, 8 blue, 3 red; 6 blue, 7 green, 5 red; 2 green, 1 red, 5 blue; 9 blue, 9 green, 5 red\nGame 9: 16 red; 5 blue, 6 red, 9 green; 7 blue, 6 green, 2 red; 15 red, 5 blue, 3 green; 1 red, 6 green, 6 blue; 3 blue, 7 red, 5 green\nGame 10: 17 green, 5 blue, 6 red; 18 green, 9 red; 4 red, 4 blue, 4 green; 10 red, 6 green, 5 blue; 8 red, 4 blue, 12 green\nGame 11: 4 blue, 2 green, 5 red; 1 blue, 1 red; 9 blue, 1 green, 2 red; 4 red, 10 blue; 3 green, 4 blue, 3 red\nGame 12: 4 green, 2 blue, 7 red; 4 blue, 2 green, 1 red; 7 green, 5 blue, 9 red\nGame 13: 1 green, 3 red, 3 blue; 1 blue, 10 green; 2 green, 3 blue\nGame 14: 7 red, 3 green, 12 blue; 5 red, 4 green, 6 blue; 13 blue, 1 red; 4 blue, 6 red, 2 green; 4 red, 3 blue; 9 red, 13 blue\nGame 15: 4 blue, 5 red, 2 green; 7 red, 2 blue, 1 green; 17 red, 3 blue; 2 blue; 4 blue, 8 red\nGame 16: 5 blue; 9 red, 14 green, 5 blue; 5 blue, 9 green\nGame 17: 3 blue, 5 red; 6 blue, 1 green, 4 red; 7 green, 6 blue, 7 red; 1 red, 6 blue, 4 green; 6 green; 1 blue, 6 green\nGame 18: 9 blue, 4 green, 2 red; 1 green, 9 red, 10 blue; 14 red, 10 green, 17 blue; 12 red, 1 green, 15 blue; 3 blue, 8 red, 2 green; 3 green, 11 red, 13 blue\nGame 19: 1 blue, 3 red, 9 green; 14 green, 3 red, 2 blue; 1 blue, 8 red, 11 green; 4 blue, 3 red; 14 red, 4 green; 5 red, 8 green\nGame 20: 2 blue, 3 red, 2 green; 3 blue, 2 green; 1 red, 4 green, 5 blue; 9 blue, 9 green, 3 red; 3 green, 1 blue, 4 red; 1 red, 9 green, 2 blue\nGame 21: 11 blue, 6 red; 8 red; 7 red, 6 green, 11 blue; 7 green, 7 red, 11 blue; 6 red, 12 blue\nGame 22: 7 green, 8 blue, 5 red; 12 green, 4 red, 2 blue; 12 green, 7 red, 11 blue\nGame 23: 5 green, 2 blue, 7 red; 6 blue, 8 green, 3 red; 10 red, 5 blue; 6 green, 3 blue; 1 green, 8 red\nGame 24: 17 blue, 1 green, 2 red; 2 red, 11 green, 9 blue; 6 red, 8 blue\nGame 25: 3 red, 1 blue, 19 green; 1 blue, 1 green, 6 red; 6 green, 5 blue; 4 green, 2 red, 19 blue; 6 red, 19 blue, 18 green; 1 red, 4 blue, 1 green\nGame 26: 3 red, 4 blue, 2 green; 2 red, 1 green, 3 blue; 14 blue, 1 green, 3 red; 5 green, 2 red, 10 blue; 9 blue, 2 red, 7 green; 15 blue, 4 green, 3 red\nGame 27: 3 blue, 5 red, 2 green; 6 red, 7 blue, 9 green; 14 green, 11 red, 6 blue; 3 blue, 20 green, 3 red; 6 red, 15 green, 7 blue; 13 red, 1 blue, 14 green\nGame 28: 4 blue, 7 green, 4 red; 2 red, 4 blue, 7 green; 6 blue, 11 green, 4 red; 6 blue, 6 green, 3 red; 6 green, 12 red\nGame 29: 3 red, 9 blue; 5 red, 5 blue; 2 green, 3 red, 3 blue\nGame 30: 1 green, 1 red, 3 blue; 1 blue, 1 red, 3 green; 1 blue; 1 blue, 3 green, 1 red; 2 blue, 2 green\nGame 31: 2 blue, 1 red; 1 blue, 1 green, 1 red; 4 blue, 3 green; 1 red, 3 green, 2 blue; 2 green\nGame 32: 1 blue, 6 green; 9 red, 6 green; 1 blue, 15 red, 3 green\nGame 33: 18 green, 1 blue, 10 red; 10 red, 1 blue, 7 green; 11 green; 6 red, 13 green\nGame 34: 10 red, 14 blue, 6 green; 2 green, 13 blue, 1 red; 8 green, 7 blue, 1 red; 9 blue, 7 green, 4 red\nGame 35: 5 blue, 9 green, 2 red; 7 green, 9 blue, 5 red; 1 green, 5 red\nGame 36: 10 red; 5 red, 1 green, 1 blue; 2 green, 8 red; 9 red, 2 green; 1 blue, 10 red; 6 red, 1 green, 1 blue\nGame 37: 13 red, 1 blue, 7 green; 1 green, 9 red, 3 blue; 4 red, 1 blue, 11 green; 1 red; 1 red, 1 blue; 6 red, 3 blue, 2 green\nGame 38: 3 blue, 12 red, 7 green; 1 green; 12 red, 1 blue\nGame 39: 7 green, 12 blue, 2 red; 3 red, 10 blue, 7 green; 2 red, 8 green, 3 blue; 3 red, 12 blue, 5 green\nGame 40: 7 green, 5 red; 1 green, 2 blue; 2 red, 1 green, 7 blue\nGame 41: 1 red, 7 green, 2 blue; 2 green, 2 blue; 4 blue, 7 green, 1 red; 1 blue, 1 red, 7 green; 6 blue, 2 red, 3 green\nGame 42: 6 blue; 4 green, 18 blue, 1 red; 10 green, 14 blue, 2 red; 6 blue, 4 green; 2 red, 13 blue, 6 green; 6 green, 1 red, 5 blue\nGame 43: 5 blue, 12 red; 5 blue, 2 green, 7 red; 9 red, 4 blue; 1 green, 11 red, 2 blue; 5 red, 1 green; 2 blue, 3 red, 1 green\nGame 44: 4 blue, 9 red, 4 green; 4 blue, 10 red; 4 green, 5 red; 1 green, 2 red, 3 blue\nGame 45: 7 green, 2 blue, 18 red; 19 red, 7 green; 8 green, 1 blue, 19 red; 2 green, 12 red; 6 red, 5 green; 7 green, 10 red\nGame 46: 1 blue, 15 red, 11 green; 7 red, 1 green, 5 blue; 13 red, 2 blue, 2 green; 7 green, 5 blue, 10 red; 12 green, 3 red, 1 blue\nGame 47: 2 blue, 2 red, 5 green; 7 green, 2 red, 7 blue; 10 blue, 2 red, 8 green\nGame 48: 8 green, 10 red; 6 green, 5 red; 12 green, 2 blue; 17 green, 5 red, 1 blue; 14 green, 3 blue, 16 red; 1 blue, 5 red\nGame 49: 5 blue, 6 red, 12 green; 8 blue, 15 green; 4 blue, 3 green, 3 red; 6 red, 11 green, 10 blue; 9 green, 2 red, 10 blue\nGame 50: 10 red, 11 green, 14 blue; 6 green, 8 blue, 17 red; 2 blue, 4 red; 6 blue, 8 green, 17 red; 17 red, 9 blue, 2 green; 13 blue, 16 red, 12 green\nGame 51: 12 red, 2 green, 7 blue; 5 blue, 10 red; 1 blue, 7 red, 1 green; 14 blue, 2 red, 1 green\nGame 52: 5 blue, 5 red, 8 green; 1 blue, 9 green, 7 red; 4 blue, 5 red, 3 green; 7 green, 2 blue, 2 red; 5 red, 3 blue, 17 green; 19 green, 1 red\nGame 53: 4 red, 1 blue, 2 green; 1 green; 2 red; 1 blue, 2 green; 2 green, 4 red\nGame 54: 7 red; 9 red, 2 blue, 14 green; 1 blue, 5 green; 7 green, 3 blue\nGame 55: 11 blue, 2 green; 11 blue, 9 green, 12 red; 8 green, 6 blue, 12 red\nGame 56: 2 green, 1 red, 2 blue; 4 red, 5 green, 5 blue; 5 red, 5 blue, 10 green; 8 red, 3 green, 3 blue\nGame 57: 7 red, 3 green; 1 blue, 6 red, 1 green; 1 blue; 7 red, 1 green, 1 blue; 2 red, 1 blue, 1 green; 3 green, 1 blue\nGame 58: 9 blue, 2 red; 2 green, 9 blue, 2 red; 5 blue, 4 green\nGame 59: 8 red; 5 green, 1 blue, 3 red; 1 green, 8 red\nGame 60: 9 green, 8 blue, 3 red; 10 green, 4 red; 8 blue, 2 green, 4 red; 1 red, 5 green, 7 blue; 1 green, 4 blue, 1 red; 4 blue\nGame 61: 5 blue, 9 red, 4 green; 5 green, 7 blue, 6 red; 7 green, 8 red; 7 blue, 4 red, 2 green; 8 red, 4 blue, 5 green; 3 green, 9 red, 7 blue\nGame 62: 9 red, 10 blue; 1 green, 7 red, 13 blue; 1 green, 11 blue; 6 red, 16 blue, 5 green; 20 red, 1 green, 3 blue\nGame 63: 9 red, 8 green, 1 blue; 13 green, 12 red, 1 blue; 7 green, 5 red, 3 blue\nGame 64: 3 red, 2 blue, 10 green; 3 green; 1 blue, 8 green, 2 red; 7 red, 1 blue, 4 green; 9 red, 1 blue, 4 green\nGame 65: 7 red, 6 green; 1 blue, 4 green, 7 red; 6 red; 6 red, 4 green, 1 blue\nGame 66: 4 blue, 4 green; 1 red, 7 green, 1 blue; 7 green, 3 red, 3 blue; 1 blue, 1 red, 6 green; 3 red, 7 green\nGame 67: 5 green, 16 blue, 5 red; 4 red, 7 green, 3 blue; 4 red, 4 green, 9 blue; 12 green, 5 red; 15 green, 3 red; 10 blue, 6 red, 1 green\nGame 68: 3 green, 3 blue, 5 red; 2 green, 6 blue; 2 green, 3 blue, 1 red; 1 blue, 11 red\nGame 69: 5 green, 1 blue; 16 green, 9 red; 10 red, 18 green\nGame 70: 1 blue, 1 green; 1 red; 1 red, 2 blue, 1 green; 1 green, 2 red; 2 blue, 2 red; 1 red\nGame 71: 11 red; 2 green, 3 blue, 13 red; 1 green, 3 blue; 15 red, 1 green, 3 blue; 4 red\nGame 72: 2 blue, 6 red, 18 green; 6 red, 8 green, 7 blue; 5 blue, 3 red, 12 green; 3 red, 2 blue, 4 green\nGame 73: 12 blue, 7 green, 4 red; 5 green, 2 red, 4 blue; 3 green, 3 red, 10 blue; 1 green, 12 blue, 6 red; 3 blue, 6 green, 14 red\nGame 74: 3 red; 1 blue, 8 green, 11 red; 3 green, 2 red\nGame 75: 5 green, 2 red, 1 blue; 8 green, 2 red; 11 green, 2 red; 2 red, 17 green; 3 blue, 3 green, 2 red\nGame 76: 1 blue, 5 green, 4 red; 8 green, 11 blue, 5 red; 8 blue, 2 red, 11 green\nGame 77: 1 red, 11 blue, 7 green; 8 green, 4 blue; 1 blue, 8 green\nGame 78: 1 green, 1 red, 1 blue; 3 green, 1 blue, 3 red; 10 green, 1 blue; 12 green\nGame 79: 1 red, 11 blue, 6 green; 3 green, 3 red, 5 blue; 16 blue, 1 red, 5 green; 11 blue, 3 green, 2 red; 8 blue, 6 green, 4 red\nGame 80: 5 green; 6 green, 7 red, 4 blue; 7 green, 5 blue; 6 blue, 6 green; 7 blue, 7 green; 6 green, 7 blue, 5 red\nGame 81: 1 green, 14 blue; 11 blue, 1 red; 1 red, 16 green, 2 blue; 9 green, 1 red, 13 blue; 10 green, 8 blue\nGame 82: 7 green, 7 red, 3 blue; 4 blue, 1 green, 4 red; 2 green, 14 blue, 3 red\nGame 83: 15 blue; 2 blue, 1 green, 4 red; 8 green, 4 red, 6 blue\nGame 84: 12 blue, 17 green; 6 green, 1 red, 16 blue; 1 blue, 1 red; 5 blue, 11 green\nGame 85: 5 blue, 15 green, 3 red; 4 blue, 1 green, 11 red; 8 red, 2 blue, 4 green\nGame 86: 11 blue, 16 green, 16 red; 11 blue, 17 red, 10 green; 8 green, 7 red\nGame 87: 2 red, 4 green, 2 blue; 2 blue, 6 green; 2 red, 3 blue, 3 green; 1 red, 4 green; 1 green, 2 blue, 2 red; 4 blue, 4 green\nGame 88: 10 red, 7 green; 2 blue, 6 red, 1 green; 8 blue, 8 red, 7 green; 2 green, 5 blue, 2 red; 3 blue, 3 red, 6 green\nGame 89: 9 blue, 16 green; 2 red, 5 blue, 6 green; 12 blue, 15 green; 8 green, 2 red, 3 blue\nGame 90: 18 red, 1 blue; 3 red, 5 blue, 4 green; 1 blue, 2 green, 6 red; 2 green, 16 red, 3 blue; 5 blue, 13 red, 5 green\nGame 91: 4 red, 7 green, 1 blue; 3 green, 16 blue, 2 red; 4 green, 8 blue\nGame 92: 4 red, 3 green; 5 red, 11 green, 1 blue; 16 green, 13 red; 15 green, 14 red, 3 blue; 3 red, 5 green, 2 blue\nGame 93: 2 blue, 1 red, 3 green; 10 blue, 1 red, 10 green; 11 blue, 16 green, 4 red; 2 green, 20 blue, 7 red; 11 green, 8 red, 15 blue; 9 green, 10 blue, 1 red\nGame 94: 2 blue, 12 red, 10 green; 16 red, 9 blue, 6 green; 5 green, 9 blue, 11 red; 4 red, 2 blue\nGame 95: 2 green, 9 red, 1 blue; 2 blue, 1 red; 2 green, 5 blue, 3 red\nGame 96: 1 green, 5 red, 13 blue; 1 green, 2 red, 13 blue; 2 green, 2 red, 17 blue; 3 red, 1 green; 6 red, 2 green; 1 green, 7 blue, 4 red\nGame 97: 1 green, 1 red, 1 blue; 2 blue, 11 green; 1 blue, 13 green; 9 blue, 6 green, 1 red; 10 green, 8 blue\nGame 98: 12 green, 9 red; 12 green, 10 blue, 3 red; 3 red, 13 green, 7 blue\nGame 99: 8 green, 10 blue, 1 red; 10 green, 2 red, 6 blue; 3 green, 1 blue, 1 red; 10 blue, 1 red\nGame 100: 8 blue, 6 green, 8 red; 7 red, 2 blue; 2 red, 10 green, 10 blue; 9 green, 7 red; 3 red, 7 green, 1 blue")
(println (set-text->set "3 blue, 4 red"))
(println (game-text->game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
(println (sum-ids-possible-games sample))
(println (sum-ids-possible-games input))
(println (min-color {:id 1, :sets '({:blue 3, :red 4, :green 0} {:blue 6, :red 1, :green 2} {:blue 0, :red 0, :green 2})} :blue))
(println (min-cubes {:id 1, :sets '({:blue 3, :red 4, :green 0} {:blue 6, :red 1, :green 2} {:blue 0, :red 0, :green 2})}))
(println (sum-powers-min-possible-cubes sample))
(println (sum-powers-min-possible-cubes input))

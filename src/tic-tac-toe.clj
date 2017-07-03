;; Play the game of tic-tac-toe against the computer
;;
;; Copyright (C) 2017 Johan Dykstrom
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; DESCRIPTION
;;
;; Call function start! to start a new game:
;;
;; (start! '0)  ; Start a new game playing as noughts
;; (start! 'X)  ; Start a new game playing as crosses
;;
;; Call function move! to make a move:
;;
;; (move! N)    ; Make a move in square N
;;
;; The squares on the board are numbered as follows:
;;
;; 012
;; 345
;; 678

(ns tic-tac-toe
  (:require [ysera.test :refer [is is-not is=]]))

;; The initial board state of the board, 0 always moves first
(def initial-state {:board '[- - - - - - - - -] :player '0})

;; A board state where there is one move left for a win for 0
(def one-left-state {:board '[0 - X 0 X - - - -] :player '0})

;; The entire game state is stored in an atom
(def game (atom {:user '0 :state initial-state}))

(defn generate-moves
  {:doc "Generate all possible moves on the current BOARD."
   :test #(do
            (is= (generate-moves '[- - - - - - - - -]) [0 1 2 3 4 5 6 7 8])
            (is= (generate-moves '[0 - X 0 X 0 X 0 -]) [1 8])
            (is= (generate-moves '[0 0 X 0 X 0 X X X]) []))}
  [board]
  (->> (range 9)
       (filter #(= (nth board %) '-))))

(defn board-full?
  {:doc "Return true if BOARD is full."
   :test #(do
            (is (board-full? '[0 X 0 X 0 X 0 X 0]))
            (is-not (board-full? '[0 X 0 X - X 0 X 0]))
            (is-not (board-full? '[- - - - - - - - -])))}
  [board]
  (empty? (filter #(= % '-) board)))

(defn line-of?
  {:doc "Return true if LINE is a line of SYMBOLs."
   :test #(do
            (is (line-of? '0 '[0 0 0]))
            (is (line-of? 'X '[X X X]))
            (is-not (line-of? '0 '[0 X 0]))
            (is-not (line-of? 'X '[- - -])))}
  [symbol line]
  (= [symbol symbol symbol] line))

(defn horizontal-line-of?
  {:doc "Return true if BOARD contains at least one horizontal line of SYMBOLs."
   :test #(do
            (is (horizontal-line-of? '0 '[X - X 0 0 0 - - -]))
            (is (horizontal-line-of? 'X '[X X X 0 - 0 - 0 -]))
            (is-not (horizontal-line-of? '0 '[X X X 0 - 0 - 0 -]))
            (is-not (horizontal-line-of? 'X '[X - - X 0 0 X - 0])))}
  [symbol board]
  (->> (partition 3 board)
       (some #(line-of? symbol %))
       (boolean)))

(defn vertical-line-of?
  {:doc "Return true if BOARD contains at least one vertical line of SYMBOLs."
   :test #(do
            (is (vertical-line-of? '0 '[X - 0 - - 0 X - 0]))
            (is-not (vertical-line-of? 'X '[X - 0 - - 0 X - 0])))}
  [symbol board]
  (->> (partition 3 board)
       (apply map vector) ;; Transpose
       (some #(line-of? symbol %))
       (boolean)))

(defn filter-by-index [coll idxs]
  {:doc "Returns the elements in COLL that are given by IDXS."
   :test #(do
            (is= (filter-by-index [:a :b :c :d :e] [0 2 4]) [:a :c :e]))}
  (map (partial nth coll) idxs))

(defn diagonal-line-of?
  {:doc "Return true if BOARD contains at least one diagonal line of SYMBOLs."
   :test #(do
            (is (diagonal-line-of? '0 '[X - 0 - 0 - 0 X -]))
            (is-not (diagonal-line-of? 'X '[X - 0 - - 0 X - 0])))}
  [symbol board]
  (or (= (filter-by-index board [0 4 8]) [symbol symbol symbol])
      (= (filter-by-index board [2 4 6]) [symbol symbol symbol])))

(defn has-line?
  {:doc "Return true if PLAYER has a line of three anywhere on the BOARD."
   :test #(do
            (is (has-line? 'X '[X - 0 - X - 0 0 X])))}
  [player board]
  (or (horizontal-line-of? player board)
      (vertical-line-of? player board)
      (diagonal-line-of? player board)))

(defn game-over?
  {:doc "Return true if game is over."
   :test #(do
            (is (game-over? '[X 0 X X 0 0 0 X 0]))
            (is-not (game-over? '[- - - - - - - - -])))}
  [board]
  (or (board-full? board)
      (has-line? '0 board)
      (has-line? 'X board)))

(defn invalid-move?
  {:doc "Return true if MOVE is invalid with respect to BOARD."
   :test #(do
            (is (invalid-move? -1 '[- - - - - - - - -]))
            (is (invalid-move? 9 '[- - - - - - - - -]))
            (is-not (invalid-move? 0 '[- - - - - - - - -]))
            (is (invalid-move? 0 '[0 - - - - - - - -])))}
  [move board]
  (or (< move 0) (> move 8) (not= (nth board move) '-)))

(defn switch-player
  {:doc "Return the other player."
   :test #(do
            (is= (switch-player '0) 'X)
            (is= (switch-player 'X) '0))}
  [player]
  (get {'0 'X 'X '0} player))

(defn evaluate
  {:doc "Evaluate the board and return a positive score if PLAYER is
in the lead, a negative score if the opponent is in the lead, and 0
if there is a draw."
   :test #(do
            (is= (evaluate initial-state) 0)
            (is= (evaluate {:board '[- X - 0 0 0 - X -] :player '0}) 100)
            (is= (evaluate {:board '[- X - 0 0 0 - X -] :player 'X}) -100))}
  [{:keys [board player]}]
  (cond (has-line? player board) 100
        (has-line? (switch-player player) board) -100
        :else 0))

(defn make-move
  {:doc "Make MOVE for PLAYER in the given BOARD."
   :test #(do
            (is= (make-move 4 {:board '[- - - - - - - - -] :player '0})
                 {:board '[- - - - 0 - - - -] :player 'X}))}
  [move {:keys [board player]}]
  {:board (assoc board move player) :player (switch-player player)})

;; Forward declaration of alpha-beta to use in alpha-beta-reducer
(declare alpha-beta)

(defn alpha-beta-reducer
  {:doc "Implements the alpha-beta algorithm for function alpha-beta."}
  [state {:keys [alpha beta], :as best} move]
  (let [score (- (alpha-beta (make-move move state) (- beta) (- alpha)))]
    (cond
      ;; If the score is too good, we cut off the search tree here,
      ;; because the opponent will not select this branch
      (>= score beta) (reduced {:best-move move :alpha beta :beta beta})
      ;; If we found a new best move, return that
      (> score alpha) {:best-move move :alpha score :beta beta}
      ;; The previous best move is still best
      :else best)))

(defn alpha-beta
  {:doc "Return the score of the best move for the player on the move."
   :test #(do
            (is= (alpha-beta one-left-state -999 999) 100))}
  [{board :board, :as state} alpha beta]
  (if (game-over? board)
    (evaluate state)
    (:alpha (reduce (partial alpha-beta-reducer state)
                    {:best-move -1 :alpha alpha :beta beta}
                    (generate-moves board)))))

(defn find-move
  {:doc "Find and return the best move for the player on the move."
   :test #(do
            (is= (find-move one-left-state) 6))}
  [{board :board, :as state}]
  (:best-move (reduce (partial alpha-beta-reducer state)
                      {:best-move -1 :alpha -999 :beta 999}
                      (generate-moves board))))

(defn print-board
  {:doc "Prints a representation of the board on stdout."}
  [board]
  (doseq [row (partition 3 board)]
    (println row)))

(defn make-engine-move
  {:doc "Makes a move for the engine, and returns updated game state."}
  [{:keys [user state]}]
  (let [move (find-move state)]
    (println "Engine move:" move)
    {:user user :state (make-move move state)}))

(defn make-user-move
  {:doc "Makes a move for the user, and returns updated game state."}
  [move {:keys [user state]}]
  (println "User move:" move)
  {:user user :state (make-move move state)})

(defn start!
  {:doc "Starts a new game with user playing the given symbol."}
  [user]
  (if (not (or (= user '0) (= user 'X)))
    (println "Invalid user. Allowed values are '0 and 'X:" user)
    (do
      (println "New game")
      (swap! game (fn [_] {:user user :state initial-state}))
      (when (= user 'X)
        (swap! game make-engine-move))
      (print-board (get-in @game [:state :board])))))

(defn get-result
  {:doc "Returns the result of the game."}
  [board]
  (cond (has-line? '0 board) "The winner is 0."
        (has-line? 'X board) "The winner is X."
        (board-full? board) "The game is drawn."
        :else "The game is not finished."))

(defn print-result
  {:doc "Prints the result of the game."}
  [board]
  (println "Game over." (get-result board)))

(defn move!
  {:doc "Makes the given MOVE for the user, and then makes a move for the engine,
while also checking if the game is over."}
  [move]
  (let [board (get-in @game [:state :board])]
    (cond (game-over? board)
          (println "Game is already over. Please start a new game.")
          (invalid-move? move board)
          (println "Invalid move:" move)
          :else
          (do
            (swap! game (partial make-user-move move))
            (if (game-over? (get-in @game [:state :board]))
              (print-result (get-in @game [:state :board]))
              (do
                (swap! game make-engine-move)
                (if (game-over? (get-in @game [:state :board]))
                  (print-result (get-in @game [:state :board])))))
            (print-board (get-in @game [:state :board]))))))

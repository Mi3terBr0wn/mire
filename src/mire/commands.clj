(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Command functions

(defn grab
  "Pick something up."
  [thing]
  (dosync
    (cond
      (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
      (if (room-contains-gold? @*current-room* thing)
        (do
          (case thing
            "coin"
            (do (alter *money* inc) (change-points 1))
            "bagmoney"
            (do (alter *money* + 7) (change-points 7))
            "treasuregold"
            (do (alter *money* + 15) (change-points 15)))
          (if (= ((keyword thing) @(:gold @*current-room*)) 1)
            (alter (:gold @*current-room*) dissoc (keyword thing))
            (do
              (def temp-gold ((keyword thing) @(:gold @*current-room*)))
              (alter (:gold @*current-room*) dissoc (keyword thing))
              (alter (:gold @*current-room*) assoc (keyword thing) (- temp-gold 1))))
            (str " You picked up the " thing ".")
          )
          (str " There isn't any " thing " here.")
        )

        (room-contains? @*current-room* thing)
        (case thing
          "arrows" (do
                     (.set player/*arrows* (+ (.get player/*arrows*) 5))
                     (move-delete (keyword thing) (:items @*current-room*))
                     (println "You picked up arrows.")
                     )
          (do
            (move-between-refs (keyword thing)
                               (:items @*current-room*)
                               *inventory*)
            (str "You picked up the " thing ".")
            )
          )
        :default (str "There isn't any " thing " here.")
      )

    )
  )

(defn seemoney
  "See your money"
  []
  (str (join "\r\n" (map #(str "Money is "% " .\r\n") [(str @*money*)])))
)

(def commands
  {"move" move,
   "north" (fn [] (move :north)),
   "south" (fn [] (move :south)),
   "east" (fn [] (move :east)),
   "west" (fn [] (move :west)),
   "grab" grab
   "seemoney" seemoney
   "discard" discard
   "inventory" inventory
   "detect" detect
    "look" look
    "say" say
    "players" players
    "help" help
    "attack" attack
    "buy" buy
    "deadplayer" deadplayer
   "shoot" shoot
   })

(defn shoot
  "Shoot another player"
  [target]
  (dosync
    (if (player/carrying? :bow)
      (if (> (.get player/*arrows*) 0)
        (if (contains? @health target)
          (if (contains? @(:inhabitants @*current-room*) target)
            (do
              (commute health assoc target (- (@health target) 50))
              (.set player/*arrows* (- (.get player/*arrows*) 1))
              "Great shot!"
              )
            "No such target in the room."
            )
          "Target doesn't exist."
        )
        "You don't have arrows."
      )
      "You don't have a bow."
    )
  )
)

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\r\n"
       (join "\r\n" (seq @*inventory*))
       "\nYou have " (.get player/*arrows*) " arrows."
       )
  )

(defn attack
  "Attack other player"
  [target]
  (dosync
    (if (contains? @health target)
      (if (contains? @(:inhabitants @*current-room*) target)
        (do
          (if (not= (@lives target) "dead")

            (do
              (commute health assoc target (- (@health target) damage) )
              (if (< (int(@health target)) 1)
                ((commute lives assoc target "dead")
                 (println
                   (say (str target " killed by " *name* "\r\n")))
                 (commute score assoc *name* (+ (@score *name*) 25)))
                )

              "Successful attack.")
            "He is dead")
          )
          "No such target in the room."
        )
        "Target doesn't exist."
      )
    )
)

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join "\n" (map #(str "There is " % " here.\n")
                           @(:items @player/*current-room*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))

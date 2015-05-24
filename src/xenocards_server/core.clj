(ns xenocards-server.core
  (:require [org.httpkit.server :refer :all]
            [xenocards-server.game :as game])
  (:gen-class))

(declare exit-room)

(def registered-names (atom {}))
(def users (atom {}))
(def rooms (atom {}))
(def channels (atom {}))
(def nextroomid (atom 0))

(defn string-to-int [string]
  (Integer/parseInt (clojure.string/replace string #"[^0-9]" "")))

(defmacro defcommands [name channel commanddata & body]
  `(defn ~name [~channel ~commanddata]
      (if (not= ~commanddata "")
       (let [~'command (clojure.string/split ~commanddata #" ")]
        (condp = (~'command 0)
         ~@(mapcat (fn [commandhandler#]
            `(~(nth commandhandler# 0)
               (if ~(if (= (nth commandhandler# 1) 0) 'true `(not= (count ~'command) ~(nth commandhandler# 1)))
                   (send! ~channel "invalid number of arguments")
                  ~(nth commandhandler# 2))))
                     (partition 3 body))
           (send! ~channel "unsupported command"))))))

(defcommands receive-room channel commanddata
  "exit" 1
    (do
      (exit-room channel)
      (send! channel "exited room"))
  "roomid" 1
    (send! channel (str ((@users channel) :room)))
  "roomname" 1
    (send! channel ((@rooms ((@users channel) :room)) :name))
  "myrole" 1
    (send! channel (if (= ((@rooms ((@users channel) :room)) :host) channel) "host" "opponent"))
  "ready" 1
    ;TODO ready
    (send! channel "ready")
  "opponentsname" 1
    (let [room (@rooms ((@users channel) :room))]
      (if (= (room :host) channel)
        (if (contains? room :opponent)
          (send! channel ((@users (room :opponent)) :name))
          (send! channel "no opponent"))
        (send! channel ((@users (room :host)) :name)))))

(defcommands receive-lobby channel commanddata
  "listplayers" 1
    (send! channel (clojure.string/join " " (map :name (vals @users))))
  "listrooms" 1
    (send! channel (clojure.string/join "\n"
      (map
        (fn [key]
          (str key " " ((@rooms key) :name)))
        (keys @rooms))))
  "myname" 1
    (send! channel ((@users channel) :name))
  "createroom" 2
    (if (contains? @rooms (command 1))
      (send! channel (str "room " (command 1) "already exists"))
      (let [roomid (swap! nextroomid inc)]
        (swap! rooms assoc roomid
          {:host channel
           :name (command 1)})
        (swap! users assoc channel
          (assoc (@users channel)
            :room roomid
            :handler receive-room))
        (send! channel (str "room " (command 1) " created"))))
  "joinroom" 2
    (let [roomid (string-to-int (command 1))]
      (if-let [room (@rooms roomid)]
        (if (contains? room :opponent)
          (send! channel "occupied")
          (do
            (swap! rooms assoc roomid
              (assoc (@rooms roomid)
                :opponent channel))
            (swap! users assoc channel
              (assoc (@users channel)
                :room roomid
                :handler receive-room))
            (send! channel (str "entered room " ((@rooms roomid) :name)))))
        (send! channel "no room with this name"))))

(defcommands receive-signin channel commanddata
  "authorize" 3
    (if-let [password (@registered-names (command 1))]
      (if (= password (command 2))
        (do
          (if (contains? @channels (command 1))
            (let [previous-channel (@channels (command 1))]
              (swap! users assoc channel
                (@users previous-channel))
              (swap! users dissoc previous-channel))
              ;TODO resume game when reconnecting
            (swap! users assoc channel
              {:name (command 1)
               :handler receive-lobby}))
          (swap! channels assoc (command 1) channel)
          (send! channel "connected"))
        (send! channel "invalid password"))
      (send! channel "not found"))
  "register" 3
    (if (contains? @registered-names (command 1))
      (send! channel "failure")
      (do
        (swap! registered-names assoc (command 1) (command 2))
        (send! channel "success"))))



(defn exit-room [channel]
  (if-let [roomid ((@users channel) :room)]
    (let [role (if (= ((@rooms roomid) :host) channel) :host :opponent)]
      (if (= role :opponent)
        (swap! rooms assoc roomid (dissoc (@rooms roomid) :opponent))
        (if-let [opponent ((@rooms roomid) :opponent)]
          (do
            (swap! rooms assoc roomid
              (dissoc
                (assoc
                  (@rooms roomid)
                  :host opponent)
                :opponent))
            (send! opponent "you are host now"))
          (swap! rooms dissoc roomid)))))
  (swap! users assoc channel
    (dissoc
      (assoc (@users channel)
        :handler receive-lobby)
      :room)))



(defn server [request]
  (with-channel request channel
    (swap! users assoc channel {:handler receive-signin})
    (send! channel "welcome")
    (on-close channel
      (fn [status]
        (exit-room channel)
        ;TODO pause current game on disconnect instead of disconnecting
        (swap! users dissoc channel)))
    (on-receive channel (fn [data] (((@users channel) :handler) channel data)))))

(defn -main [& args]
  (run-server server {:port 9090}))

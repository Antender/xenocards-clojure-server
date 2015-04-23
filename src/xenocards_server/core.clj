(ns xenocards-server.core
  (:use org.httpkit.server)
  (:gen-class))

(declare exit-room)

(def handlers (atom {}))

(def registered-names (atom {}))

(def users (atom {}))

(def rooms (atom {}))

(def userlocations (atom {}))

(defmacro defcommands [name channel commanddata & body]
  `(defn ~name [~channel ~commanddata]
      (if (not= ~commanddata "")
       (let [~'command (clojure.string/split ~commanddata #" ")]
        (condp = (~'command 0)
         ~@(mapcat (fn [commandhandler#]
            `(~(nth commandhandler# 0)
               (if (and (not= ~(nth commandhandler# 1) 0) (not= (count ~'command) ~(nth commandhandler# 1)))
                   (send! ~channel "invalid number of arguments")
                  ~(nth commandhandler# 2))))
                     (partition 3 body))
           (send! ~channel "unsupported command"))))))

(defcommands receive-room channel commanddata
  "exit" 1
    (do
      (exit-room channel)
      (send! channel "exited room")
      (swap! userlocations dissoc channel))
  "roomname" 1
    (send! channel (@userlocations channel))
  "myrole" 1
    (send! channel (if (= ((@rooms (@userlocations channel)) :host) (@users channel)) "host" "opponent"))
  "opponentsname" 1
    (let [role (if (= ((@rooms (@userlocations channel)) :host) (@users channel)) :host :opponent)]
        (if-let [name ((@rooms (@userlocations channel)) (if (= role :host) :opponent :host))]
          (send! channel name)
          (send! channel "no opponent"))))

(defcommands receive-lobby channel commanddata
  "listplayers" 1
    (send! channel (clojure.string/join " " (vals @users)))
  "listrooms" 1
    (send! channel (clojure.string/join " " (keys @rooms)))
  "myname" 1
    (send! channel (@users channel))
  "createroom" 2
    (if (@rooms (command 1))
      (send! channel (str "room " (command 1) "already exists"))
      (do
        (swap! rooms assoc (command 1) {:host (@users channel)})
        (swap! userlocations assoc channel (command 1))
        (swap! handlers assoc channel receive-room)
        (send! channel (str "room " (command 1) " created"))))
  "joinroom" 2
    (if-let [room (@rooms (command 1))]
      (if (contains? room :opponent)
        (send! channel "occupied")
        (do
          (swap! rooms assoc (command 1) {:opponent (@users channel)})
          (swap! userlocations assoc channel (command 1))
          (swap! handlers assoc channel receive-room)
          (send! channel (str "entered room " (command 1)))))
      (send! channel "no room with this name")))

(defcommands receive-signin channel commanddata
  "connect" 3
    (if-let [password (@registered-names (command 1))]
      (if (= password (command 2))
        (do
          (swap! users assoc channel (command 1))
          (swap! handlers assoc channel receive-lobby)
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
  (if-let [roomname (@userlocations channel)]
    (let [role (if (= ((@rooms roomname) :host (@users channel))) :host :opponent)]
      (swap! rooms assoc roomname (dissoc (@rooms roomname) role))
      (if (not (or (contains? (@rooms roomname) :host) (contains? (@rooms roomname) :opponent)))
        (swap! rooms dissoc roomname))))
      (swap! handlers assoc channel receive-lobby))



(defn handler [request]
  (with-channel request channel
    (swap! handlers assoc channel receive-signin)
    (send! channel "welcome")
    (on-close channel
      (fn [status]
        (exit-room channel)
        ;TODO pause current game on disconnect
        (swap! handlers dissoc channel)
        (swap! users dissoc channel)
        (swap! userlocations dissoc channel)))
    (on-receive channel (fn [data] ((get @handlers channel) channel data)))))

(defn -main [& args]
  (run-server handler {:port 9090}))

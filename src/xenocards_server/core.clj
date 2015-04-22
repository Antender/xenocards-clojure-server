(ns xenocards-server.core
  (:use org.httpkit.server)
  (:gen-class))

(def handlers (atom{}))

(def registered-names (atom {}))

(def usernames (atom {}))

(defn receive-lobby [channel data]
  (let [command (clojure.string/split data #" ")]
    (cond
      (= (command 0) "list") (send! channel (clojure.string/join " " (vals @usernames)))
      (= (command 0) "myname") (send! channel (get @usernames channel)))))

(defn receive-signin [channel data]
  (let [command (clojure.string/split data #" ")]
    (cond
      (= (command 0) "connect")
        (if-let [password (@registered-names (command 1))]
          (if (= password (command 2))
            (do
              (swap! usernames assoc channel (command 1))
              (swap! handlers assoc channel receive-lobby)
              (send! channel "connected"))
            (send! channel "invalid password"))
          (send! channel "not found"))
      (= (command 0) "register")
        (if (contains? @registered-names (command 1))
          (send! channel "failure")
          (do
            (swap! registered-names assoc (command 1) (command 2))
            (send! channel "success"))))))

(defn handler [request]
  (with-channel request channel
    (swap! handlers assoc channel receive-signin)
    (on-close channel (fn [status] (swap! usernames dissoc channel)))
    (on-receive channel (fn [data] ((get @handlers channel) channel data)))))

(defn -main [& args]
  (run-server handler {:port 9090}))

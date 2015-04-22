(ns xenocards-server.core
  (:use org.httpkit.server)
  (:gen-class))

(def clients (atom {}))

(defn receive-command [channel data]
  (let [command (clojure.string/split data #" ")]
    (cond
      (= (command 0) "list") (send! channel (clojure.string/join " " (vals @clients)))
      (= (command 0) "myname") (send! channel (get @clients channel))
      (= (command 0) "rename") (swap! clients assoc channel (command 1)))))

(defn handler [request]
  (with-channel request channel
    (swap! clients assoc channel "Anonymous")
    (on-close channel (fn [status] (swap! clients dissoc channel)))
    (on-receive channel (fn [data] (receive-command channel data)))))


(defn -main [& args]
  (run-server handler {:port 9090}))

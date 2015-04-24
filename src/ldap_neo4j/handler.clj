(ns ldap-neo4j.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :as middleware]
            [ring.util.response :refer [resource-response response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ldap-neo4j.core :as core]))

(defroutes app-routes
  (GET "/person/:id" [id] (response (core/id->tree id)))
  (GET "/person-by-mail/:mail" [mail] (response (core/mail->tree mail)))
  (GET "/autocomplete/:s" [s] (response (core/autocomplete s)))
  (GET "/" [] "Hello World")
  (route/not-found "Not Found"))

(def app
  (middleware/wrap-json-body
    (middleware/wrap-json-response
      app-routes)))


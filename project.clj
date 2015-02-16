(defproject ldap-neo4j "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojurewerkz/neocons "3.0.0"]
                 [tranchis/clj-ldap "0.0.9a"]
                 [clj-http "1.0.1"]
                 [org.clojure/data.json "0.2.5"]
                 [ring/ring-json "0.3.1"]
                 [compojure "1.3.1"]
                 [tranchis/clojure-snippets "0.1.1"]
                 [ring/ring-defaults "0.1.2"]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler ldap-neo4j.handler/app
         :port 3100})

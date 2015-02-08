(ns ldap-neo4j.core
  (:require [clj-ldap.client :as ldap]
            [clojure.core.async :as async :refer [go <!! >! chan alts!! close!]]
            [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.cypher :as cy]))

(def sample-set
  [{:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=tesla,dc=example,dc=com", :uid "newton", :mail "newton@ldap.forumsys.com", :dn "uid=newton,dc=example,dc=com", :sn "Newton", :cn "Issac Newton"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "einstein", :mail "einstein@ldap.forumsys.com", :dn "uid=einstein,dc=example,dc=com", :sn "Einstein", :cn "Albert Einstein"}
   {:objectClass #{"top" "person" "inetOrgPerson" "posixAccount" "organizationalPerson"}, :uid "tesla", :gidNumber "99999", :mail "tesla@ldap.forumsys.com", :homeDirectory "home", :dn "uid=tesla,dc=example,dc=com", :sn "Tesla", :uidNumber "88888", :cn "Nikola Tesla"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "galieleo", :mail "galieleo@ldap.forumsys.com", :dn "uid=galieleo,dc=example,dc=com", :sn "Galilei", :cn "Galileo Galilei"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "euler", :mail "euler@ldap.forumsys.com", :dn "uid=euler,dc=example,dc=com", :sn "Euler", :cn "Leonhard Euler"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "gauss", :mail "gauss@ldap.forumsys.com", :dn "uid=gauss,dc=example,dc=com", :sn "Gauss", :cn "Carl Friedrich Gauss"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "riemann", :mail "riemann@ldap.forumsys.com", :dn "uid=riemann,dc=example,dc=com", :sn "Riemann", :cn "Bernhard Riemann"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :uid "euclid", :mail "euclid@ldap.forumsys.com", :dn "uid=euclid,dc=example,dc=com", :sn "Euclid", :cn "Euclid"}
   {:objectClass #{"top" "person" "inetOrgPerson" "organizationalPerson"}, :manager "uid=newton,dc=example,dc=com", :dn "cn=read-only-admin,dc=example,dc=com", :userPassword "{SHA}W6ph5Mm5Pz8GgiULbPgzG37mj9g=", :sn "Read Only Admin", :cn "read-only-admin"}
   {:initials "TS", :objectClass #{"top" "inetOrgPerson" "posixAccount"}, :manager "uid=newton,dc=example,dc=com", :o "Company", :displayName "Test", :uid "test", :gidNumber "0", :homeDirectory "home", :dn "uid=test,dc=example,dc=com", :givenName "Test", :sn "Test", :uidNumber "24601", :cn "Test"}])

(defn lazy-channels [ch]
  (let [elem (<!! ch)]
    (if (not (nil? elem))
      (lazy-seq (cons elem (lazy-channels ch))))))

(defn node [conn key]
  (let [n (cy/tquery conn
                     "MATCH (n) WHERE n.dn = {dn} RETURN n"
                     {:dn key})]
    (if (empty? n)
      (nn/create conn {:dn key})
      (nn/get conn (:id (:metadata (first (vals (first n)))))))))

(defn rel! [conn from to link]
  (let [r (cy/tquery conn
                      (str "MATCH r = (a)-[" link "]->(b) WHERE "
                           "a.dn = {dnfrom} AND b.dn = {dnto} RETURN r")
                      {:dnfrom (:dn (:data from))
                       :dnto (:dn (:data to))})]
    (if (empty? r)
      (nrl/create conn from to link))))

(defn register->node [conn identity-key parent-key register]
  (let [dn (identity-key register)
        parent (parent-key register)
        i-node (node conn dn)]
    (if (not (nil? parent))
      (let [p-node (node conn parent)]
        (rel! conn i-node p-node parent-key)))))

(defn ldap-seq [host bind-dn password root-dn filter]
  (let [ch (chan)]
    (let [ldap-server (ldap/connect {:host host
                                     :bind-dn bind-dn
                                     :password password})]
      (ldap/search! ldap-server
                    root-dn
                    {:scope "sub"
                     :filter (str "(objectclass=" filter ")")}
                    (fn [x] (go (>! ch x))))
      (close! ch))
    (lazy-channels ch)))

(defn ldap->neo4j [host bind-dn password root-dn filter identity-key parent-key]
  (let [conn (nr/connect "http://localhost:7474/db/data/")]
    (map #(register->node conn identity-key parent-key %) sample-set
         #_(ldap-seq host bind-dn password root-dn filter))))

(ldap->neo4j "ldap.forumsys.com"
             "cn=read-only-admin,dc=example,dc=com"
             "password"
             "dc=example,dc=com"
             "person"
             :dn
             :manager)


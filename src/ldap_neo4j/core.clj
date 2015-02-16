(ns ldap-neo4j.core
  (:require [clj-ldap.client :as ldap]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [snippets-generic :as sg]
            [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.labels :as nl]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.cypher :as cy]))

(def props (sg/load-props "ldap_client.properties"))

(defn ldap-auth []
  (let [host (:ldap.host props)
        password (:ldap.pass props)
        username (:ldap.user props)]
    (ldap/connect {:host host
                   :bind-dn username
                   :password password})))

(defn ldap-seq [host username password root-dn filter attrs]
  (let [ldap-server (ldap-auth)]
    (ldap/search-all ldap-server
                     root-dn
                     {:scope "sub"
                      :attributes (read-string (:ldap.attrs props))
                      :filter (str "(objectclass=" (:ldap.entity props) ")")})))

(defn id->person [id]
  (let [ldap-server (ldap-auth)
        filter (str "(&" (str "(objectclass=" (:ldap.entity props) ")")
                    "(distinguishedName="
                    (clojure.string/replace id #"\\," "\\\\5c,")
                    "))")]
    (first (ldap/search ldap-server
                        (:ldap.rootdn props)
                        {:scope "one"
                         :attributes (read-string (:ldap.attrs props))
                         :filter filter}))))

(defn url->node-id [url]
  (java.lang.Integer/parseInt
    (last (clojure.string/split url #"/"))))

(defn rel->annotated-node [conn rel]
  (let [node-type (keyword (:type rel))]
    {:type node-type :content (nn/get conn (url->node-id (:end rel)))}))

(defn fill-node! [conn node]
  (let [d-name (:displayName (:data node))]
    (if (nil? d-name)
      (let [ldap-info (id->person
                        ((keyword (:ldap.identity props)) (:data node)))]
        (nn/update conn node ldap-info))
      true)))

(defn id->node [conn identity-key identity-value]
  (let [n (cy/tquery conn
                     (str "MATCH (n) WHERE n."
                          (name identity-key)
                          " =~ {" (name identity-key)
                          "} RETURN n")
                     {identity-key identity-value})
        node-id (:id (:metadata (get (first n) "n")))]
    (if (nil? node-id)
      nil
      (nn/get conn node-id))))

(defn id->nodes [conn identity-key identity-value]
  (let [n (cy/tquery conn
                     (str "MATCH (n) WHERE n."
                          (name identity-key)
                          " =~ {" (name identity-key)
                          "} RETURN n")
                     {identity-key identity-value})
        node-id (map (fn [a] (:id (:metadata a))) (map #(get % "n") n))]
    (if (empty? node-id)
      nil
      (map #(nn/get conn %) node-id))))

(defn id->children [conn identity-key identity-value children-key]
  (let [n (cy/tquery conn
                     (str "MATCH (n)-[" children-key "]->(c) WHERE n."
                          (name identity-key)
                          " = {" (name identity-key)
                          "} RETURN c")
                     {identity-key identity-value})]
    (map #(get % "c") n)))

(defn node->tree [conn node]
  (fill-node! conn node)
  (let [rels (json/read-str (:body (client/get (:relationships-uri node)))
                            :key-fn keyword)]
    (let [m (map #(rel->annotated-node conn %)
                 (filter #(= (:id node)
                             (url->node-id (:start %)))
                         rels))
          by-type (let [grouped (group-by :type m)]
                    (zipmap (keys grouped)
                            (map #(map :data (map :content %))
                                 (vals grouped))))
          node-colleagues (flatten
                            (map #(id->children
                                    conn
                                    (keyword (:ldap.identity props))
                                    ((keyword (:ldap.identity props)) %)
                                    (keyword (:ldap.children props)))
                                 ((keyword (:ldap.parent props)) by-type)))]
      (doall (map #(fill-node! conn %) (map :content m)))
      (doall (map #(fill-node! conn %) node-colleagues))
      (merge by-type {:colleagues (map :data node-colleagues)
                        :origin (:data node)}))))

(defn node [conn node-type identity-key identity-value register]
  (let [n (id->node conn node-type identity-key identity-value)]
    (if (nil? n)
      (let [new-node (nn/create conn register)]
        (if (not (nil? (node-type register)))
          (nl/add conn new-node (node-type register)))
        new-node)
      (let [old-node n]
        (nn/update conn
                   old-node
                   register)
        (if (not (nil? (node-type register)))
          (nl/add conn old-node (node-type register)))
        old-node))))

(defn rel! [conn identity-key from to link]
  (let [r (cy/tquery conn
                      (str "MATCH r = (a)-[" link "]->(b) WHERE "
                           "a." (name identity-key)
                           " = {" (name identity-key) "from} "
                           "AND b." (name identity-key) " = {"
                           (name identity-key)
                           "to} RETURN r")
                      {(keyword (str (name identity-key) "from"))
                       (identity-key (:data from))
                       (keyword (str (name identity-key) "to"))
                       (identity-key (:data to))})]
    (if (empty? r)
      (nrl/create conn from to link))))

(defn register->node [conn node-type identity-key
                      parent-key children-key register]
  (let [dn (identity-key register)
        parent (parent-key register)
        children (children-key register)
        i-node (node conn node-type identity-key dn register)]
    (if (not (nil? parent))
      (let [p-node (node conn node-type
                         identity-key parent {identity-key parent})]
        (rel! conn identity-key i-node p-node parent-key)))
    (if (not (nil? children))
      (dorun
        (map (fn [c]
               (try
                 (let [c-node (node conn node-type
                                    identity-key c {identity-key c})]
                   (rel! conn identity-key i-node c-node children-key))
                 (catch Exception e
                   #_ (do nothing))))
             children)))))

(defn ldap->neo4j [host bind-dn password root-dn
                   filter attrs node-type identity-key
                   parent-key children-key]
  (let [conn (nr/connect "http://localhost:7474/db/data/")]
    (map #(register->node conn node-type identity-key
                          parent-key children-key %)
         #_sample-set
         (ldap-seq host bind-dn password root-dn filter attrs))))

(defn id->tree [id]
  (let [conn (nr/connect "http://localhost:7474/db/data/")
        n (id->node conn (:ldap.identity props) id)]
    (if (nil? n)
      {}
      (do
        (node->tree conn n)
        (node->tree conn n)))))

(defn mail->tree [mail]
  (let [conn (nr/connect "http://localhost:7474/db/data/")
        n (id->node conn :mail (str "(?i)" mail))]
    (if (nil? n)
      {}
      (do
        (node->tree conn n)
        (node->tree conn n)))))

(defn autocomplete [s]
  (let [conn (nr/connect "http://localhost:7474/db/data/")
        n (id->nodes conn :mail (str "(?i)" s ".*"))]
    n))

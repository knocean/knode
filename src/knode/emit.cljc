(ns knode.emit
  (:require [knode.core :as core]
            [hiccup.core :as html]))

;;;;;;;;;; TTL emission
(defn statement->ttl [env statement]
  [(:predicate statement) (:object statement)])

(defn stanza->ttl [env form]
  (case (:type form)
    (:prefix :base :graph :label) [(str (:origin form) " .")]
    :blank-line [(:origin form)]
    :stanza (concat [(str "<" (:target (:subject form)) ">")]
                    (map (partial statement->ttl env) (:statements form))
                    ["."])
    [(str "TODO" (:type form) (:subject form))]))

(defn forms->stanzas [forms]
  (let [subject-map (reduce
                     (fn [memo form]
                       (if-let [subject (:subject form)]
                         (update memo subject (fnil conj []) form)
                         memo))
                     {} forms)]
    ((fn rec [forms]
       (when (not (empty? forms))
         (let [head (first forms)
               tail (rest forms)]
           (lazy-seq
            (case (:type head)
              :subject (cons {:type :stanza
                              :subject head
                              :statements (get subject-map (:target head))}
                             (rec tail))
              :statement (rec tail)
              (cons head (rec tail)))))))
     forms)))

(defn emit-ttl [env forms]
  (mapcat (partial stanza->ttl env) (forms->stanzas forms)))

;;;;;;;;;; HTML emission
(defn emit-html [env forms]
  (html/html [:p "Hello world"]))

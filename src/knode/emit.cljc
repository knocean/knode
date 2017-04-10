(ns knode.emit
  (:require [knode.core :as core]
            [hiccup.core :as html]))

;;;;;;;;;; TTL emission
(defn link->ttl [env link]
  (case (:type link)
    :prefixed-name (str (:prefix link) ":" (:name link))
    :label-name (let [label (get-in env [:labels (:name link)])]
                  (if-let [curie (:curie label)]
                    curie
                    (str "<" (:iriref label) ">")))
    :string (:string link)
    :iri (str "<" (:iriref link) ">")))

(defn statement->ttl [env statement]
  (let [object (:object statement)]
    (str (link->ttl env (:predicate statement)) ": "
         (link->ttl env object)
         (if-let [tp (get-in statement [:object :datatype])]
           (str "^^" (link->ttl env tp)))
         (when (= :label-name (:type object))
           (str " # " (:name object))))))

(defn stanza->ttl [env form]
  (case (:type form)
    (:prefix :base :graph) [(str (:origin form) " .")]
    :label []
    :blank-line [(:origin form)]
    :stanza (concat [(link->ttl env (:target (:subject form)))]
                    (map
                     #(str %1 %2)
                     (cons "  " (repeat "; "))
                     (map (partial statement->ttl env) (:statements form)))
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

(defn emit-ttl [env+forms]
  (clojure.string/join \newline (mapcat
                                 (partial stanza->ttl (:env env+forms))
                                 (forms->stanzas (:forms env+forms)))))

;;;;;;;;;; HTML emission
(defn emit-html [env forms]
  (html/html [:p "Hello world"]))

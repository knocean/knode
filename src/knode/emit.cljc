(ns knode.emit
  (:require [clojure.string :as string]
            [knode.core :as core]))

(defn emit-ttl-statement
  "Given a block-map with :predicate and :object,
   return a Turtle statement string."
  [{:keys [predicate object] :as block}]
  (str
   (:curie predicate)
   " "
   (cond
     (:language object)
     (format "\"%s\"%s" (:lexical object) (:language object))
     (:datatype object)
     (format "\"%s\"^^%s" (:lexical object) (get-in object [:datatype :curie]))
     (:lexical object)
     (format "\"%s\"" (:lexical object))
     (:curie object)
     (:curie object)
     :else
     (format "<%s>" (:iri object)))))

(defn emit-ttl
  "Given a sequence of context block-maps (prefixes),
   a subject block-map, and a sequence of block-maps (statements),
   return a stand-alone Turtle string
   with prefixes and a stanza for that subject with those statements."
  [context-blocks subject blocks]
  (str
   (->> context-blocks
        (filter :prefix)
        (map #(str "@prefix " (:prefix %) ": <" (:iri %) "> ."))
        (string/join "\n"))
   "\n\n"
   (:curie subject)
   "\n  "
   (->> blocks
        (filter :predicate)
        (map emit-ttl-statement)
        (string/join "\n; "))
   "\n."))

(defn emit-rdfa-statement
  "Given a block-map with :predicate and :object,
   return a list item using Hiccup for HTML+RDFa."
  [{:keys [predicate object] :as block}]
  [:li
   [:a {:href (:iri predicate)} (:label predicate)]
   ": "
   (cond
     (:language object)
     [:span
      {:property (:curie predicate)
       :xml:lang (string/replace (:language object) #"^@" "")}
      (:lexical object)]

     (:datatype object)
     [:span
      {:property (:curie predicate)
       :datatype (get-in object [:datatype :curie])}
      (:lexical object)]

     (:lexical object)
     [:span {:property (:curie predicate)} (:lexical object)]

     :else
     [:a
      {:href (:iri object) :property (:curie predicate)}
      (or (:label object) (:curie object) (:iri object))])])

(defn emit-rdfa
  "Given a sequence of context block-maps (prefixes),
   a subject block-map, and a sequence of block-maps (statements),
   return an HTML+RDFa <ul> element in Hiccup format."
  [context-blocks subject blocks]
  (apply
   conj
   [:ul
    {:prefixes
     (->> context-blocks
          (filter :prefix)
          (map #(str "prefix " (:prefix %) ": " (:iri %)))
          (string/join "\n"))
     :resource (:curie subject)}]
   (->> blocks
        (filter :predicate)
        (map emit-rdfa-statement)
        vec)))

(ns knode.emit
  (:require [knode.core :as core]
            [hiccup.core :as html]))

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

;;;;;;;;;; TTL emission
(defn link->ttl [env link]
  (case (:type link)
    :prefixed-name (str (:prefix link) ":" (:name link))
    :label-name (let [label (get-in env [:labels (:name link)])]
                  (if-let [curie (:curie label)]
                    curie
                    (str "<" (:iriref label) ">")))
    :string (str \" (:string link) \")
    :iri (str "<" (:iriref link) ">")))

(defn statement->ttl [env statement]
  (let [object (:object statement)]
    (str (link->ttl env (:predicate statement)) " " (link->ttl env object)
         (if-let [tp (get-in statement [:object :datatype])]
           (str "^^" (link->ttl env tp)))
         (when (= :label-name (:type object))
           (str " # " (:name object))))))

(defn stanza->ttl [env form]
  (case (:type form)
    (:prefix :base :graph) [(str (:origin form) " .")]
    :label []
    :blank-line [(:origin form)]
    :stanza (concat ["" (link->ttl env (:target (:subject form)))]
                    (map
                     #(str %1 %2)
                     (cons "  " (repeat "; "))
                     (map (partial statement->ttl env) (:statements form)))
                    ["."])))

(defn emit-ttl [env+forms]
  (clojure.string/join \newline (mapcat
                                 (partial stanza->ttl (:env env+forms))
                                 (forms->stanzas (:forms env+forms)))))

(defn emit-statement2
  [{:keys [predicate object] :as block}]
  (println "PRED" predicate)
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

(defn emit-ttl2
  [context-blocks subject blocks]
  (clojure.string/join
   \newline
   (concat
    (->> context-blocks
         (filter :prefix)
         (map #(str "@prefix " (:prefix %) ": <" (:iri %) "> .")))
    [""]
    [(get-in subject [:subject :curie])]
    (->> blocks
         (filter :predicate)
         (map emit-statement2)))))

;;;;;;;;;; HTML emission
(defn forms->prefixes [forms]
  (filter #(= :prefix (:type %)) forms))

(defn link->html-link [env link]
  (case (:type link)
    :label-name (get-in env [:labels (:name link) :iriref])
    (:prefixed-name :iri) (:iriref link)
    :string nil))

(defn link->html-property [env link]
  (case (:type link)
    :prefixed-name (str (:prefix link) ":" (:name link))
    :label-name (let [label (get-in env [:labels (:name link)])]
                  (or (:curie label) (:iriref label)))
    :string (:string link)
    :iri (:iriref link)))

(defn statement->html-object [env statement]
  (let [obj (:object statement)
        ref (:iriref obj)
        prop (link->html-property env (:predicate statement))]
    (case (:type obj)
      :prefixed-name [:a {:href ref :property prop} (:curie obj)]
      :label-name (let [label (get-in env [:labels (:name obj)])]
                    [:a {:href ref :property prop} (:name obj)])
      :string [:span {:property prop} (:string obj)]
      :iri [:a {:href ref} ref])))

(defn stanza->html [env form]
  (case (:type form)
    (:blank-line :prefix :base :graph :label) nil
    :stanza [:ul {:resource (link->html-property env (:target (:subject form)))}
             (map (fn [statement]
                    [:li
                     [:a {:href (link->html-link env (:predicate statement))}
                      (link->html-property env (:predicate statement))]
                     (statement->html-object env statement)])
                  (:statements form))]))

(defn emit-html [env+forms]
  ;;(html/html [:p "Hello world"])
  (let [prefixes (forms->prefixes (:forms env+forms))
        prefix-prop (clojure.string/join
                     \newline
                     (map
                      #(str (:name %) ": " (:iriref (:target %)))
                      prefixes))]
    [:html
     [:body {:prefix prefix-prop}
      (filter
       #(not (nil? %))
       (map
        (partial stanza->html (:env env+forms))
        (forms->stanzas (:forms env+forms))))]]))

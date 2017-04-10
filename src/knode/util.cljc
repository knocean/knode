(ns knode.util)

(defn starts-with? [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = target prefix))))

(defn ends-with? [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = (reverse target) (reverse prefix)))))

(defn absolute-uri-string? [s]
  "Cribbed from http://stackoverflow.com/a/19709846/190887. Takes a string
   and returns true if it represents an absolute uri (false otherwise)"
  (not (not (re-find #"(?i)^(?:[a-z]+:)?//" s))))

(defn expand-iri [base iri]
  (if (absolute-uri-string? iri)
    iri
    (str base iri)))

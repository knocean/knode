(ns knode.front-end.util)

(defn log! [& things]
  (apply js/console.log (map clj->js things)))

(defn tap!
  ([thing]
   (log! thing)
   thing)
  ([label thing]
   (log! label thing)
   thing))

(defn blink! [selector & {:keys [delay color] :or {delay 500 color "#FFFF9C"}}]
  (let [$el (js/$ selector)
        origin (or (-> $el (.css "background-color")) "initial")]
    (-> $el
        (.stop) (.css "background-color" color)
        (.fadeTo 100 0.3 (fn [] (-> $el (.fadeTo delay 1.0 (fn [] (-> $el (.css "background-color" origin))))))))))

(defn dom-loaded [fn]
  (.addEventListener js/document "DOMContentLoaded" fn))

(defn $get
  ([uri on-done] ($get uri {} on-done))
  ([uri params on-done]
   (-> js/$
       (.get uri (clj->js params))
       (.done on-done))))

(defn $get-json
  ([uri on-done] ($get-json uri {} on-done))
  ([uri params on-done]
   ($get uri params (fn [data] (on-done (js->clj (.parse js/JSON data)))))))

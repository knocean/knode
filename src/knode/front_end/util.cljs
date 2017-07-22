(ns knode.front-end.util)

(defn blink! [selector & {:keys [delay color] :or {delay 500 color "#FFFF9C"}}]
  (let [$el (js/$ selector)
        origin (or (-> $el (.css "background-color")) "initial")]
    (-> $el
        (.stop) (.css "background-color" color)
        (.fadeTo 100 0.3 (fn [] (-> $el (.fadeTo delay 1.0 (fn [] (-> $el (.css "background-color" origin))))))))))

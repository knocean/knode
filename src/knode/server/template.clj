(ns knode.server.template
  (:require
   [hiccup.page :as pg]

   [knode.state :refer [state]]
   [knode.server.util :as sutil]))

;; ## Base Template

(defn stylesheet
  [name]
  [:link {:href (str "/assets/" name) :rel "stylesheet"}])

(defn base-template
  [req {:keys [title error message content] :as data}]
  (pg/html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:meta {:name "description" :content ""}]
    [:meta {:name "author" :content ""}]

    [:title (or title (when error "Error"))]

    (map
     stylesheet
     ["bootstrap.min.css"
      "ie10-viewport-bug-workaround.css"
      "style.css"])

    "<!--[if lt IE 9]>" [:script "/assets/ie8-responsive-file-warning.js"] "<![endif]-->"
    [:script {:src "/assets/ie-emulation-modes-warning.js"}]

    ;; HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries
    "<!--[if lt IE 9]>"
    [:script {:src "https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"}]
    [:script {:src "https://oss.maxcdn.com/respond/1.4.2/respond.min.js"}]
    "<![endif]-->"]

   [:body
    [:div {:class "container"}
     [:nav {:class "navbar navbar-default"}
      [:div {:class "container-fluid"}
       [:div {:class "navbar-header"}
        [:button {:type "button" :class "navbar-toggle collapsed" :data-toggle "collapse" :data-target "#navbar" :aria-expanded "false" :aria-controls "navbar"}
         [:span {:class "sr-only"} "Toggle navigation"]
         [:span {:class "icon-bar"}]
         [:span {:class "icon-bar"}]
         [:span {:class "icon-bar"}]]
        [:a {:class "navbar-brand" :href "/"} (:idspace @state)]]
       [:div {:id "navbar" :class "navbar-collapse collapse"}
        [:ul {:class "nav navbar-nav"}
         [:li [:a {:href (str "/ontology/" (:idspace @state))} "Terms"]]]
        (if (sutil/login? req)
          [:ul {:class "nav navbar-nav navbar-right"}
           (if-let [name (get-in req [:session :name])]
             [:li [:a {:href "/logout"} name  " (Log out)"]]
             [:li [:a {:href "/login-google"} "Log in"]])])]]]

     (when error
       [:p {:id "error"} error])
     (when message
       [:p {:id "message"} message])
     (when content
       [:div {:id "content"} content])]

    [:script {:src "/assets/jquery.min.js"}]
    [:script {:src "/assets/bootstrap.min.js"}]
    [:script {:src "/assets/ie10-viewport-bug-workaround.js"}]
    [:script {:src "/js/main.js"}]]))

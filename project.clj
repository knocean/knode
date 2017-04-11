(defproject knode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]

                 [org.clojure/data.csv "0.1.3"]
                 [hiccup "1.0.5"]

                 [http-kit "2.1.18"]
                 [compojure "1.5.1"]]

  :main knode.cli
  :aot [knode.cli])

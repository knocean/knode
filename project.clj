(defproject knode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]

                 [environ "1.1.0"]
                 [hiccup "1.0.5"]
                 [markdown-clj "0.9.89"]
                 [io.forward/yaml "1.0.6"]
                 [http-kit "2.1.18"]
                 [compojure "1.5.1"]]

  :main knode.cli
  :aot [knode.cli])

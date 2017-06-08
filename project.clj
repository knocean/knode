(defproject knode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.xml "0.0.8"]

                 [tempfile "0.2.0"]
                 [digest "1.4.5"]
                 [me.raynes/fs "1.4.6"]

                 [environ "1.1.0"]
                 [hiccup "1.0.5"]
                 [markdown-clj "0.9.89"]
                 [io.forward/yaml "1.0.6"]
                 [http-kit "2.1.18"]
                 [ring/ring-mock "0.3.0"]
                 [compojure "1.5.1"]
                 [oauth-clj "0.1.15"]

                 [com.blazegraph/bigdata-core "2.1.4"]]

  :main knode.cli
  :aot [knode.cli])

(defproject com.knocean/knode "0.2.0-SNAPSHOT"
  :description "Knowledge Development Environment"
  :url "https://github.com/knocean/knode"
  :license {:name "BSD 3-Clause License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :plugins [[lein-cljsbuild "1.1.6"]]
  :hooks [leiningen.cljsbuild]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.217"]
                 [org.clojure/test.check "0.9.0"]

                 [environ "1.1.0"]
                 [http-kit "2.1.18"]
                 [com.cemerick/url "0.1.1"]
                 [bidi "2.1.3"]
                 [hiccup "1.0.5"]
                 [enlive "1.1.6"]
                 [org.apache.jena/jena-core "3.7.0"]
                 [org.apache.jena/jena-arq "3.7.0"]
                 [org.apache.jena/jena-iri "3.7.0"]
                 [org.apache.jena/jena-tdb "3.7.0"]

                 [org.clojure/java.jdbc "0.7.6"]
                 [org.xerial/sqlite-jdbc "3.21.0"]
                 [org.postgresql/postgresql "42.2.2"]

                 [markdown-clj "0.9.89"]
                 [cheshire "5.8.0"]
                 [me.raynes/fs "1.4.6"]
                 [oauth-clj "0.1.15"]
                 [io.forward/yaml "1.0.6"]

                 [clj-jgit "0.8.10"]

                 [org.knotation/knotation-cljc "0.2.0-SNAPSHOT"]]
  :cljsbuild {:builds [{:source-paths ["src/com/knocean/knode/front_end"]
                        :compiler {:output-to "resources/public/js/knode.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        :jar true}]}
  :main com.knocean.knode.cli
  :aot [com.knocean.knode.cli])

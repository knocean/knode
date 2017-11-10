(defproject knode "0.1.0-SNAPSHOT"
  :description "Knowledge Development Environment"
  :url "https://github.com/knocean/knode"
  :license {:name "BSD 3-Clause License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :plugins [[lein-cljsbuild "1.1.6"]]
  :hooks [leiningen.cljsbuild]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.521"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.xml "0.0.8"]

                 [trivial-warning "1.0.3"]
                 [tempfile "0.2.0"]
                 [digest "1.4.5"]
                 [me.raynes/fs "1.4.6"]
                 [me.raynes/conch "0.8.0"]

                 [clucy "0.4.0"]

                 [environ "1.1.0"]
                 [hiccup "1.0.5"]
                 [crate "0.2.4"]
                 [markdown-clj "0.9.89"]
                 [io.forward/yaml "1.0.6"]
                 [http-kit "2.1.18"]
                 [ring/ring-mock "0.3.0"]
                 [bidi "2.1.2"]
                 [compojure "1.5.1"]
                 [oauth-clj "0.1.15"]
                 [clj-jgit "0.8.9"]

                 [com.blazegraph/bigdata-core "2.1.4"]]

  :cljsbuild {:builds [{:source-paths ["src/knode/front_end/query_editor"]
                        :compiler {:output-to "resources/public/js/query_editor.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        :jar true}
                       {:source-paths ["src/knode/front_end/tree_view"]
                        :compiler {:output-to "resources/public/js/tree_view.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        :jar true}
                       {:source-paths ["src/knode/front_end/text_search"]
                        :compiler {:output-to "resources/public/js/text_search.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        :jar true}]}
  :main knode.cli
  :aot [knode.cli])

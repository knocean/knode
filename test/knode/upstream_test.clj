(ns knode.upstream-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            digest
            [tempfile.core :refer [with-tempfile tempfile]]

            [knode.upstream :refer :all]))

(def lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed sed facilisis quam. Phasellus euismod euismod ipsum a luctus. Proin et libero ac purus sagittis sodales. Vestibulum vehicula eros eu leo elementum lobortis. Nullam sagittis elit sed libero elementum, vel gravida nunc maximus. Ut vehicula quam nunc. Nullam diam ligula, rhoncus sed varius et, imperdiet interdum libero. Suspendisse at malesuada libero. Cras sagittis est augue, a congue dolor tristique sed. Vivamus egestas dignissim augue non aliquam. Nullam interdum dolor eget dolor convallis finibus. Proin lacinia, nisi non pellentesque dictum, nulla ligula elementum dolor, ut accumsan leo nunc nec lectus.

  Donec bibendum eleifend ante sit amet aliquam. Quisque ut lacus purus. Sed iaculis ut ante sed tincidunt. Nulla non est luctus, porta ex a, finibus mi. Phasellus posuere lacus in suscipit efficitur. Sed eleifend lectus quis elit luctus, et pellentesque purus rhoncus. Sed eget imperdiet sapien. Sed eleifend vel enim non porttitor. Suspendisse posuere mi in odio auctor, quis consequat est pellentesque. Praesent vitae libero eu sapien laoreet efficitur. Aliquam imperdiet sem at magna bibendum, eget venenatis mauris tempor. Vestibulum varius nisi eu mi sollicitudin, sit amet tempus enim fringilla. Vestibulum vel purus eu augue tincidunt suscipit id sit amet sapien. Curabitur ullamcorper laoreet lacinia. Cras lorem augue, scelerisque ut turpis id, tincidunt commodo est. Phasellus at mi cursus, placerat lacus eget, pellentesque magna.
")

(deftest test-gzipped
  (with-tempfile [raw (tempfile lorem)
                  gzipped (tempfile)
                  other-gzipped (tempfile)
                  doubled (tempfile)]
    (spit-gzipped! gzipped lorem)
    (spit-gzipped! other-gzipped lorem)
    (testing "slurp-gzipped on a gzipped file returns the original input to spit-gzipped!"
      (is (= (slurp raw) (slurp-gzipped gzipped))))
    (testing "The zipped file is smaller than the normal one"
      (is (> (.length raw) (.length gzipped))))
    (testing "Two compressed files with the same content hash equivalently"
      (is (= (digest/sha-256 (io/file gzipped))
             (digest/sha-256 (io/file other-gzipped)))))
    (testing "Spitting to an existing file overwrites rather than appending"
      (do (spit-gzipped! doubled lorem)
          (spit-gzipped! doubled lorem)
          (is (= (slurp raw) (slurp-gzipped doubled)))))))

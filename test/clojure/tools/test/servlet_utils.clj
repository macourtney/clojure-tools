(ns clojure.tools.test.servlet-utils
  (:use clojure.test
        clojure.tools.servlet-utils)
  (:require [clojure.tools.logging :as logging]))

(deftest test-servlet-uri-path
  (is (nil? (servlet-uri-path nil))))
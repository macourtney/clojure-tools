(ns clojure.tools.test.string-utils
  (:use clojure.test
        clojure.tools.string-utils))

(deftest test-prefill
  (is (= (prefill "Blah" 4 "!") "Blah"))
  (is (= (prefill "Blah" 5 "!") "!Blah"))
  (is (= (prefill "Blah" 6 "!") "!!Blah"))
  (is (= (prefill "Blah" 7 "!") "!!!Blah"))
  (is (= (prefill "Blah" -1 "!") "Blah"))
  (is (= (prefill nil 6 "!") "!!!!!!"))
  (is (= (prefill "Blah" 6 nil) "Blah"))
  (is (= (prefill "Blah" nil "!") "Blah")))

(deftest test-str-keyword
   (is (= (str-keyword :test) "test"))
   (is (= (str-keyword "test") "test"))
   (is (= (str-keyword nil) nil)))
   
(deftest test-strip-ending
   (is (= (strip-ending "Blah" "h") "Bla"))
   (is (= (strip-ending "Blah" "ah") "Bl"))
   (is (= (strip-ending "Blah" "lah") "B"))
   (is (= (strip-ending "Blah" "Blah") ""))
   (is (= (strip-ending "Blah" "") "Blah"))
   (is (= (strip-ending "Blah" nil) "Blah"))
   (is (= (strip-ending "" "Blah") ""))
   (is (= (strip-ending nil "Blah") nil))
   (is (= (strip-ending nil nil) nil)))
   
(deftest test-add-ending-if-absent
  (is (= (add-ending-if-absent "blah" ".foo") "blah.foo"))
  (is (= (add-ending-if-absent "blah.foo" ".foo") "blah.foo"))
  (is (= (add-ending-if-absent "blah" nil) "blah"))
  (is (= (add-ending-if-absent nil ".foo") ".foo"))
  (is (= (add-ending-if-absent "blah" "") "blah"))
  (is (= (add-ending-if-absent "" ".foo") ".foo")))
  
(deftest test-str-replace-pair
  (is (= (str-replace-pair "foo.bar" ["." " dot "]) "foo dot bar"))
  (is (= (str-replace-pair "foo-bar" ["." " dot "]) "foo-bar"))
  (is (= (str-replace-pair nil ["." " dot "]) nil))
  (is (= (str-replace-pair "foo.bar" nil) "foo.bar"))
  (is (= (str-replace-pair nil nil) nil)))

(deftest test-str-replace-if
  (is (= (str-replace-if "foo.bar" { "." " dot " }) "foo dot bar"))
  (is (= (str-replace-if "me@foo.bar" { "." " dot ", "@" " at " }) "me at foo dot bar"))
  (is (= (str-replace-if "me@foo.bar" { "." " dot ", "@" " at ", "|" " or " }) "me at foo dot bar"))
  (is (= (str-replace-if "me@foo.bar" { }) "me@foo.bar"))
  (is (= (str-replace-if "me@foo.bar" nil) "me@foo.bar"))
  (is (= (str-replace-if nil { "." " dot " }) nil))
  (is (= (str-replace-if nil nil) nil)))

(deftest test-human-readable
  (is (= (human-readable "foo-bar") "foo bar"))
  (is (= (human-readable "foo_bar") "foo bar"))
  (is (= (human-readable "foo_bar-baz") "foo bar baz"))
  (is (= (human-readable "foo") "foo"))
  (is (= (human-readable "") ""))
  (is (= (human-readable nil) nil)))

(deftest test-str-to-map
  (is (= (str-to-map "foo=bar") { "foo" "bar" }))
  (is (= (str-to-map "foo=bar;baz=boz") { "foo" "bar", "baz" "boz" }))
  (is (= (str-to-map "foo=bar;baz=boz;buz=bez") { "foo" "bar", "baz" "boz", "buz" "bez" }))
  (is (= (str-to-map "foo=bar|baz=boz|buz=bez" #"\|") { "foo" "bar", "baz" "boz", "buz" "bez" }))
  (is (= (str-to-map "foo&bar|baz&boz|buz&bez" #"\|" #"\&") { "foo" "bar", "baz" "boz", "buz" "bez" }))
  (is (= (str-to-map "") {}))
  (is (nil? (str-to-map nil))))

(deftest test-escape-str
  (is (= (escape-str "\\") "\\\\"))
  (is (= (escape-str "\"") "\\\""))
  (is (= (escape-str " ") " "))
  (is (= (escape-str "blah\\") "blah\\\\"))) ;"

(deftest test-form-str
  (is (= (form-str "foo") "\"foo\""))
  (is (= (form-str :foo) ":foo"))
  (is (= (form-str (char 102)) "(char 102)"))
  (is (= (form-str (symbol "foo")) "(symbol \"foo\")"))
  (is (= (form-str 2) "2"))
  (is (= (form-str 2.3) "2.3"))
  (is (= (form-str { :foo "bar" }) "{ :foo \"bar\" }"))
  (is (= (form-str [:foo "bar"]) "[:foo \"bar\"]"))
  (is (= (form-str #{:foo "bar"}) "#{:foo \"bar\"}"))
  (is (= (form-str '(:foo "bar")) "(list :foo \"bar\")")))

(deftest test-title-case-word
  (is (= (title-case-word "foo") "Foo"))
  (is (= (title-case-word "1") "1"))
  (is (= (title-case-word "") ""))
  (is (= (title-case-word " ") " "))
  (is (= (title-case-word "\t") "\t"))
  (is (= (title-case-word nil) nil)))

(deftest test-matches
  (is (= (matches "foo bar" #"\s+") [[" " 3 4]]))
  (is (= (matches "foo 1 bar" #"\s+") [[" " 3 4] [" " 5 6]]))
  (is (= (matches "foo 1\tbar" #"\s+") [[" " 3 4] ["\t" 5 6]]))
  (is (= (matches "foo 1 bar" #"\s+" 4) [[" " 5 6]]))
  (is (= (matches "foo" #"\s+") nil))
  (is (= (matches "" #"\s+") nil))
  (is (= (matches "foo" #"\s+" nil) nil))
  (is (= (matches nil #"\s+") nil))
  (is (= (matches nil #"\s+" 0) nil))
  (is (= (matches nil #"\s+" nil) nil))
  (is (= (matches "foo" nil) nil))
  (is (= (matches "foo" nil 0) nil))
  (is (= (matches "foo" nil nil) nil))
  (is (= (matches nil nil) nil))
  (is (= (matches nil nil nil) nil)))

(deftest test-split-with-delimiters
  (is (= (split-with-delimiters "foo bar" #"\s+") ["foo " "bar"]))
  (is (= (split-with-delimiters "foo 1 bar" #"\s+") ["foo " "1 " "bar"]))
  (is (= (split-with-delimiters "foo 1\tbar" #"\s+") ["foo " "1\t" "bar"]))
  (is (= (split-with-delimiters "foo" #"\s+") ["foo"]))
  (is (= (split-with-delimiters "" #"\s+") [""]))
  (is (= (split-with-delimiters nil #"\s+") nil))
  (is (= (split-with-delimiters "foo bar" nil) nil))
  (is (= (split-with-delimiters nil nil) nil)))

(deftest test-title-case
  (is (= (title-case "foo") "Foo"))
  (is (= (title-case "foo bar") "Foo Bar"))
  (is (= (title-case "foo 1 bar") "Foo 1 Bar"))
  (is (= (title-case "foo\t1\tbar") "Foo\t1\tBar"))
  (is (= (title-case "") ""))
  (is (= (title-case nil) nil)))

(deftest test-human-title-case
  (is (= (human-title-case "foo") "Foo"))
  (is (= (human-title-case "foo-bar") "Foo Bar"))
  (is (= (human-title-case "foo_bar") "Foo Bar"))
  (is (= (human-title-case "") ""))
  (is (= (human-title-case nil) nil)))
  
(deftest test-tokenize
  (is (= (tokenize "foo bar") ["foo" "bar"]))
  (is (= (tokenize "foo-bar" "-") ["foo" "bar"]))
  (is (= (tokenize "foo-bar|baz" "-|") ["foo" "bar" "baz"]))
  (is (= (tokenize "foo-bar-baz" "-" true) ["foo" "-" "bar" "-" "baz"]))
  (is (= (tokenize "foo-bar-baz" "-" false) ["foo" "bar" "baz"]))
  (is (= (tokenize "") nil))
  (is (= (tokenize nil) nil)))
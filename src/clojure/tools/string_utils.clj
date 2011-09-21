(ns clojure.tools.string-utils
  (:import [java.security NoSuchAlgorithmException MessageDigest]
           [java.math BigInteger]
           [java.util StringTokenizer]
           [java.util.regex Matcher])
  (:require [clojure.string :as clj-string]))

(defn
#^{:doc "If the string's length does not equal total-length then this method returns a new string with length 
total-length by adding fill-char multiple times to the beginning of string. If string's length is already total-length,
then this method simply returns it."}
  prefill [string total-length fill-char]
  (let [base-string (if string string "")
        final-length (if total-length total-length 0)]
    (if (>= (. base-string length) final-length)
      base-string
      (str (clj-string/join
        (map
          (fn [index] fill-char) 
          (range (- final-length (. base-string length))))) base-string))))
        
(defn
#^{:doc "Converts a keyword to it's string value. Basically, it just removes the ':' from the beginning."}
  str-keyword [incoming-keyword]
  (if (keyword? incoming-keyword)
    (name incoming-keyword)
    (when incoming-keyword
      (str incoming-keyword))))

(defn
#^{:doc "If string ends with the string ending, then remove ending and return the result. Otherwise, return string."}
  strip-ending [string ending]
  (if (and string ending (. string endsWith ending))
    (let [ending-index (- (. string length) (. ending length))]
      (. string substring 0 ending-index))
    string))

(defn
#^{:doc "If the given string does not end with ending, then add ending to it."}
  add-ending-if-absent [string ending]
    (if string
      (if ending
        (if (. string endsWith ending)
          string
          (str string ending))
        string)
      ending))

(defn
#^{ :doc "For the given replace pair, replace all occurences of (first replace-pair) in string with 
(second replace-pair) if (second replace-pair) is not nil." }
  str-replace-pair [string replace-pair]
  (if string
    (let [target (first replace-pair)
          replacement (second replace-pair)]
      (if replacement
        (. string replace (str-keyword target) replacement)
        string))
    nil))

(defn
#^{ :doc "For each key in replace-map, substitute the value in replace map for all occurrences of the key in string." }
  str-replace-if 
  ([string replace-map]
    (reduce str-replace-pair string replace-map)))

(defn
#^{ :doc "Replaces any underscores or dashes in the given string to spaces. If string is a keyword, it is converted to a
string before the spaces are added." }
  human-readable [string]
  (if string
    (clj-string/replace (str-keyword string) #"[_-]" " ")
    string))

(defn md5-sum
  "Compute the hex MD5 sum of a list of strings."
  [& strings]
  (let [alg (doto (MessageDigest/getInstance "MD5")
              (.reset))]
    (try
      (do
        (doall (map #(. alg update (.getBytes %)) strings))
        (.toString (new BigInteger 1 (.digest alg)) 16))
      (catch NoSuchAlgorithmException e
        (throw (new RuntimeException e))))))

(defn
#^{ :doc "Converts the given string to a map using separator to separate the key value pairs, and equals-separator to 
separate the key from the value." }
  str-to-map 
  ([string] (str-to-map string #";"))
  ([string separator] (str-to-map string separator #"="))
  ([string separator equals-separator]
    (if (and string separator equals-separator)
      (reduce 
        (fn [new-map pair]
          (if (empty? (first pair))
            new-map
            (assoc new-map (.trim (first pair)) (second pair))))
        {}
        (map 
          #(filter 
            (fn [equals-seq] (not (re-matches equals-separator equals-seq))) 
            (clj-string/split % equals-separator))
          (filter 
            (fn [equals-pair] (not (re-matches separator equals-pair))) 
            (clj-string/split string separator))))
      nil)))

(defn
#^{ :doc "Java escapes the given string." }
  escape-str [string]
  (apply str
    (map 
      (fn [character] (or (char-escape-string character) character))
      string)))

(defn
#^{ :doc "Converts the given form into a string which can later be parsed using read-string." }
  form-str [form]
  (cond
    (char? form) (str "(char " (int form) ")")
    (float? form) (str form)
    (integer? form) (str form)
    (keyword? form) (str form)
    (list? form) (str "(list " (clj-string/join " " (map form-str form)) ")")
    (map? form) (str "{ " (clj-string/join ", " (map (fn [pair] (str (form-str (first pair)) " " (form-str (second pair)))) form)) " }")
    (set? form) (str "#{" (clj-string/join " " (map form-str form)) "}") 
    (string? form) (str "\"" (escape-str form) "\"")
    (symbol? form) (str "(symbol \"" form "\")")
    (vector? form) (str "[" (clj-string/join " " (map form-str form)) "]")))
    
(defn
#^{ :doc "If the given word starts with a lower case letter, then this function capitalizes the first letter. 
Otherwise, this method simply returns the given word." }
  title-case-word [word]
  (if (and word (> (.length word) 0) (re-matches #"^[a-z].*" word))
    (apply str (. Character toUpperCase (first word)) (rest word))
    word))

(defn
#^{ :doc "Returns a lazy seq of vectors which include the match information for each matching pattern of re in string." }
  matches
  ([string re] (matches string re 0))
  ([string re index]
    (when (and re string index)
      (let [matcher (if (instance? Matcher re) re (.matcher re string))]
        (when (.find matcher index)
          (lazy-seq
            (cons [(.group matcher) (.start matcher) (.end matcher)] (matches string matcher (.end matcher)))))))))

(defn group-splits [string group1 group2]
  (let [delimiter-start (second group2)
        delimiter-end (nth group2 2)
        token-string (.substring string (nth group1 2) delimiter-start)]
    (if (< delimiter-start delimiter-end)
      [token-string (.substring string delimiter-start delimiter-end)]
      [token-string])))

(defn
#^{ :doc "Returns a sequence of strings by splitting the given string by the re. Unlike clojure.string.split, this
function includes the delimiters." }
  split-with-delimiters
  ([string re] (split-with-delimiters string re true))
  ([string re return-delimiters?]
    (when (and string re)
      (let [groups (matches string re)]
        (mapcat (partial group-splits string)
          (cons [nil 0 0] groups)
          (concat groups [[nil (.length string) (.length string)]]))))))

(defn
#^{ :doc "Converts the given string to title case by capitalizing each word in the string." }
  title-case [string]
  (when string
    (clj-string/join (map title-case-word (split-with-delimiters string #"\s+")))))

(defn
#^{ :doc "Converts the given string human readable format then title cases the result." }
  human-title-case [string]
  (title-case (human-readable string)))

(defn
#^{ :doc "Tokenizes the given string into a seq." }
  tokenize 
  ([string] 
    (if string (enumeration-seq (new StringTokenizer string))))
  ([string delimiters] 
    (if string (enumeration-seq (new StringTokenizer string delimiters))))
  ([string delimiters return-delimiters?]
    (if string (enumeration-seq (new StringTokenizer string delimiters return-delimiters?)))))

(defn
#^{ :doc "Strips the quotes from around the given string if they exist." }
  strip-quotes [string]
  (if string
    (let [last-char-index (dec (.length string))]
      (if (and (= (.substring string 0 1) "\"") (= (.substring string last-char-index) "\""))
        (.substring string 1 last-char-index)
        string))))

(defn
#^{ :doc "Null safe toLowerCase." }
  lower-case [string]
  (when string
    (.toLowerCase string)))

(defn
#^{ :doc "Null safe endsWith." }
  ends-with? [string ending]
  (when string
    (if ending
      (.endsWith string ending)
      true)))

(defn
#^{ :doc "Converts the given reader into a sequence of characters." }
  reader-character-seq [reader]
  (map char (take-while #(>= % 0) (repeatedly #(.read reader)))))

(defn
#^{ :doc "Reads all of the characters in the given reader and returns the result as a string." }
  reader-string [reader]
  (clj-string/join (reader-character-seq reader)))
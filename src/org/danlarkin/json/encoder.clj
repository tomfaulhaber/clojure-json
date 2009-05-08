;; Copyright (c) 2008 Dan Larkin
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns org.danlarkin.json.encoder
  (:use clojure.contrib.pprint)
  (:import (java.io Writer StringWriter)))

(def escape-map
     #^{:private true}
     {
      \u0008 "\\b"
      \u0009 "\\t"
      \u000A "\\n"
      \u000C "\\f"
      \u000D "\\r"
      \u0022 "\\\""
      \u005C "\\\\"      
      })

(defn- escaped-char
  "Given a char, return either the char or an escaped representation.  If a character
   must be escaped and there is a shortened 'backslash' escape sequence available, it
   is used.  Otherwise the character is escaped as backslash-u-4-hex-digits.  The /
   (solidus) character can be escaped with a backslash but that is not required and
   this code does not."
  [#^Character c]
  (let [quick-escape (escape-map c)]
    (cond
     quick-escape quick-escape
     (or (= c (char 0x20)) (= c (char 0x21))) c
     (and (>= (.compareTo c (char 0x23)) 0) (<= (.compareTo c (char 0x5B)) 0)) c
     (>= (.compareTo c (char 0x5D)) 0) c
     :else (format "\\u%04X" (int c)))))

(defn- escaped-str
  "Returns an escaped (per RFC4627, section 2.5) version of the input string"
  [#^String string]
  (apply str (map escaped-char string)))

(defmulti json-dispatch
  "The pretty print dispatch function to format obj in JSON format"
  {:arglists '[[obj]]}
  class)

(defmethod json-dispatch java.lang.Boolean [obj]
  (write-out obj))
(defmethod json-dispatch nil [obj]
  (write-out 'null))
(defmethod json-dispatch java.lang.String [obj]
  ((formatter-out "\"~a\"") (escaped-str obj)))
(defmethod json-dispatch clojure.lang.Keyword [obj]
  ((formatter-out "\"~a\"") (escaped-str (name obj))))
(defmethod json-dispatch clojure.lang.Symbol [obj]
  ((formatter-out "\"~a\"") (escaped-str (name obj))))

(def json-array (formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>"))
(def json-map (formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>"))

(use-method json-dispatch clojure.lang.ISeq json-array)
(use-method json-dispatch clojure.lang.IPersistentVector json-array)
(use-method json-dispatch clojure.lang.IPersistentMap json-map)

(defmethod json-dispatch :default [obj]
  (if (number? obj)
    (print obj)
    (throw (Exception. "Undefined  data type")))) ; TODO better error message

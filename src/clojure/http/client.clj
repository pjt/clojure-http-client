(ns clojure.http.client
  (:use [clojure.contrib.java-utils :only [as-str]]
        [clojure.contrib.duck-streams :only [read-lines spit]]
        [clojure.contrib.str-utils :only [str-join]])
  (:import (java.net URL URLEncoder)
           (java.io StringReader InputStream)))

(def default-headers {"User-Agent" (str "Clojure/" (clojure-version)
                                        " (+http://clojure.org)"),
                      "Connection" "close"})

(defn url-encode
  "Wrapper around java.net.URLEncoder returning a (UTF-8) URL encoded
representation of argument, either a string or map."
  [arg]
  (if (map? arg)
    (str-join \& (map #(str-join \= (map url-encode %)) arg))
    (URLEncoder/encode (as-str arg) "UTF-8")))

(defn- send-body
  [body connection headers]
  (.setDoOutput connection true)
  ;; this isn't perfect, since it doesn't account for
  ;; different capitalization etc
  (when (and (map? body)
             (not (contains? headers "Content-Type")))
    (.setRequestProperty connection
                         "Content-Type"
                         "application/x-www-form-urlencoded"))

  (.connect connection)

  (let [out (.getOutputStream connection)]
    (cond
      (string? body) (spit out body)
      (map? body) (spit out (url-encode body))
      (instance? InputStream body) (let [bytes (make-array Byte/TYPE 1000)]
                                     (loop [bytes-read (.read body bytes)]
                                       (when (pos? bytes-read)
                                         (.write out bytes 0 bytes-read)
                                         (recur (.read body bytes))))))
    (.close out)))

(defn url
  "Returns java.net.URL from args. First, required, arg is a string or
URL object; optional keyword args for :query & :fragment are accepted.
E.g. (url url-base :query {:debug true} :fragment \"footer\"). Queries 
& fragments are url-encoded.
  
As shortcuts, when url is called with 2 arguments they are interpreted as 
<url-arg>, <query>; when called with 3, they are interpreted as <url-arg>, 
<query>, <fragment>, with no need for the :query & :fragment keywords."
  ([u] (if (instance? URL u) u (URL. u)))
  ([u query] (URL. (str u \? (url-encode query))))
  ([u opt & opts]
   (if-not (keyword? opt)
     (URL. (str u \? (url-encode opt) \# (url-encode (first opts))))
     (let [opts (apply hash-map (cons opt opts))
           query (when-let [q (:query opts)]     (str \? (url-encode q)))
           frag  (when-let [f (:fragment opts)]  (str \# (url-encode f)))]
       (URL. (str u query frag))))))

(defn- body-seq
  "Returns a lazy-seq of lines from either the input stream
or the error stream of connection, whichever is appropriate."
  [connection]
  (read-lines (or (if (>= (.getResponseCode connection) 400)
                    (.getErrorStream connection)
                    (.getInputStream connection))
                  (StringReader. ""))))

(defn- parse-headers
  "Returns a map of the response headers from connection."
  [connection]
  (let [hs (.getHeaderFields connection)]
    (into {} (for [[k v] hs :when k] [k (first v)]))))

(defn- parse-cookies
  "Returns a map of cookies when given the Set-Cookie string sent
by a server."
  [cookie-string]
  (when cookie-string
    (into {}
      (for [cookie (.split cookie-string ";")]
        (let [keyval (map #(.trim %) (.split cookie "="))]
          [(first keyval) (second keyval)])))))

(defn- create-cookie-string
  "Returns a string suitable for sending to the server in the
\"Cookie\" header when given a clojure map of cookies."
  [cookie-map]
  (str-join "; " (map (fn [cookie]
                        (str (as-str (key cookie))
                             "="
                             (as-str (val cookie))))
                      cookie-map)))

(defn request
  "Perform an HTTP request on url u. "
  [u & [method headers cookies body]]
  (let [connection (.openConnection (url u))
        method (.toUpperCase (as-str (or method
                                         "GET")))]
    (.setRequestMethod connection method)

    (doseq [header (conj default-headers (or headers {}))]
      (.setRequestProperty connection
                           (first header)
                           (second header)))

    (when (and cookies (not (empty? cookies)))
      (.setRequestProperty connection
                           "Cookie"
                           (create-cookie-string cookies)))
    (if body
      (send-body body connection headers)
      (.connect connection))

    (let [headers (parse-headers connection)]
      {:body-seq (body-seq connection)
       :code (.getResponseCode connection)
       :msg (.getResponseMessage connection)
       :method method
       :headers (dissoc headers "Set-Cookie")
       ;; This correctly implements case-insensitive lookup.
       :get-header #(.getHeaderField connection (as-str %))
       :cookies (parse-cookies (headers "Set-Cookie"))
       :url (str (.getURL connection))})))

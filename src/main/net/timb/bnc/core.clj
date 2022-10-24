(ns net.timb.bnc.core
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.xml :as xml]
            ))

(def corpus-root-path "bnc/download/Texts")

(def unpacked-corpus-resource
  (io/as-file (io/resource corpus-root-path)))

(defn xml-file? [f]
  (-> f .getName (s/ends-with? ".xml")))

(def corpus-files (file-seq unpacked-corpus-resource))
(def corpus-xml-files (filter xml-file? corpus-files))

(defn extract-words- [x]
  (if (string? x)
    nil
    (case (:tag x)
      :w (->> x
              :content
              (map s/trim)
              ;(map s/upper-case)
              )
      (:teiHeader :c :bibl :gap :unclear :trunc) nil
      (:bncDoc :pb :div :mw :p :head :s :u :hi :align :quote :wtext
               :vocal :shift :stext :event :pause :corr :lg :l :list
               :item
               :label :note
               
               :sp :speaker
               :stage
               )
      (mapcat extract-words- (:content x))

      (assert false ["Unknown tag" (:tag x) "in" x]))))

(defn get-xml-path [pth doc]
  (if (empty? pth)
    doc
    (when (= (:tag doc)
             (first pth))
      (some (fn [content] (get-xml-path (rest pth) content))
            (:content doc)))))
  
(defn extract-words [n x]
  (prn "#" n
       (get-xml-path [:bncDoc
                      :teiHeader
                      :fileDesc
                      :titleStmt
                      :title
                      ] x))
  (let [rv (into #{} (extract-words- x))]
    ; (prn "ended #" n (count rv))
    rv))

(defn extract-all-words [fs]
  (into (sorted-set)
        (comp
          (map xml/parse)
          (map-indexed extract-words)
          cat)
        (shuffle fs)))

(defn build-raw-word-list [{:keys [limit out-file] :as args}]
  (let [ws (extract-all-words (take limit corpus-xml-files))]
    (spit out-file (s/join "\n" ws))))

(defn process-raw-word-list [{:keys [limit out-file] :as args}]
  (let [ws (extract-all-words (take limit corpus-xml-files))]
    (spit out-file (s/join "\n" ws))))

(defn convert-to-edn [{:keys [limit out-dir] :as args}]
  (assert false "you don't want to be messin with these files, do you?")
  (time
    (let [files (take limit corpus-xml-files)
          extract-root (io/resource out-dir)]
      (assert extract-root (str "Extract root must exist in resources"))
      (let [file-map
            (into [] (map-indexed
                       #(vector %1 %2
                                (-> %2
                                    (.getPath)
                                    (s/replace-first corpus-root-path out-dir)
                                    (s/replace-first ".xml" ".edn")))
                       files))]
        (doseq [[n xml-file edn-file] file-map]
          (prn n (.getName xml-file) "->" edn-file)
          (io/make-parents edn-file)
          (time (with-open [w (clojure.java.io/writer edn-file)]
                  (binding [*out* w] (pr (xml/parse xml-file)))))
          (time (with-open [r (io/reader edn-file)]
                  (clojure.edn/read (java.io.PushbackReader. r)))))))))

(comment
  (convert-to-edn {:limit 10 :out-dir "bnc/edn"})

  (prn unpacked-corpus-resource)
  (prn (map #(.getName %) (filter xml-file? corpus-files)))
  (first corpus-xml-files)

  (let [ws (extract-words (nth corpus-xml-files 1))]
    ;(prn ws)
    (prn (count ws)))  
  (let [ws (extract-all-words (take 20 corpus-xml-files))]
    (spit "word-list.txt" (s/join "\n" ws)))
  
  (let [ws (extract-all-words corpus-xml-files)]
    (spit "word-list.txt" (s/join "\n" ws)))

  (map-indexed #(vector %1 (.getName %2)) (take 10 corpus-xml-files)))

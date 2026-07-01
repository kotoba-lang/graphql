(ns graphql.emit
  "EDN hiccup-style GraphQL definitions to SDL."
  (:require [clojure.string :as str]))

(defn- id [x] (if (keyword? x) (name x) (str x)))

(defn- gtype [t]
  (cond
    (and (vector? t) (= :list (first t))) (str "[" (gtype (second t)) "]")
    (and (vector? t) (= :list! (first t))) (str "[" (gtype (second t)) "]!")
    (keyword? t) (name t)
    :else (str t)))

(defn field
  "Compile one EDN GraphQL field definition to SDL."
  [[_ nm a b]]
  (let [[args typ] (if (map? a) [a b] [nil a])]
    (str (id nm)
         (when (seq args)
           (str "(" (str/join ", " (for [[an at] args] (str (id an) ": " (gtype at)))) ")"))
         ": " (gtype typ))))

(defn- block [fields] (str/join "\n" (map #(str "  " (field %)) fields)))

(defn item
  "Compile one EDN GraphQL definition to an SDL string."
  [form]
  (let [[op & more] form]
    (case op
      :type (let [[nm & r] more
                  opts (when (map? (first r)) (first r))
                  fields (if opts (rest r) r)]
              (str "type " (id nm)
                   (when (seq (:implements opts))
                     (str " implements " (str/join " & " (map id (:implements opts)))))
                   " {\n" (block fields) "\n}"))
      :interface (let [[nm & fields] more] (str "interface " (id nm) " {\n" (block fields) "\n}"))
      :input (let [[nm & fields] more] (str "input " (id nm) " {\n" (block fields) "\n}"))
      :enum (let [[nm & vals] more]
              (str "enum " (id nm) " {\n" (str/join "\n" (map #(str "  " (id %)) vals)) "\n}"))
      :union (let [[nm & types] more] (str "union " (id nm) " = " (str/join " | " (map id types))))
      :scalar (str "scalar " (id (first more)))
      :schema (str "schema {\n"
                   (str/join "\n" (for [[k v] (first more)] (str "  " (id k) ": " (id v)))) "\n}")
      (str (id op)))))

(defn graphql
  "Compile a sequence of GraphQL definitions to an SDL document string."
  [& defs]
  (str/join "\n\n" (map item defs)))

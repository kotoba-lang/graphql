(ns graphql.emit-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [graphql.emit :as g]
            [kotoba.graphql :as kg]))

(deftest definitions
  (is (= "id: ID!" (g/field [:field :id :ID!])))
  (is (= "user(id: ID!): User" (g/field [:field :user {:id :ID!} :User])))
  (is (= "tags: [String!]!" (g/field [:field :tags [:list! :String!]])))
  (is (= "enum Role {\n  ADMIN\n  USER\n}" (g/item [:enum :Role :ADMIN :USER])))
  (is (= "union Result = User | Post" (g/item [:union :Result :User :Post])))
  (is (= "scalar DateTime" (g/item [:scalar :DateTime])))
  (is (= "type User implements Node {\n  id: ID!\n}"
         (g/item [:type :User {:implements [:Node]} [:field :id :ID!]]))))

(deftest schema-document
  (let [src (g/graphql
              [:schema {:query :Query}]
              [:type :Query [:field :user {:id :ID!} :User] [:field :users [:list! :User!]]]
              [:type :User {:implements [:Node]}
               [:field :id :ID!] [:field :name :String!] [:field :role :Role]]
              [:interface :Node [:field :id :ID!]]
              [:enum :Role :ADMIN :USER]
              [:scalar :DateTime])]
    (is (str/starts-with? src "schema {\n  query: Query\n}"))
    (is (str/includes? src "type Query {\n  user(id: ID!): User\n  users: [User!]!\n}"))
    (is (str/includes? src "interface Node {\n  id: ID!\n}"))
    (is (str/ends-with? src "scalar DateTime"))
    (is (= src (kg/graphql
                 [:schema {:query :Query}]
                 [:type :Query [:field :user {:id :ID!} :User] [:field :users [:list! :User!]]]
                 [:type :User {:implements [:Node]}
                  [:field :id :ID!] [:field :name :String!] [:field :role :Role]]
                 [:interface :Node [:field :id :ID!]]
                 [:enum :Role :ADMIN :USER]
                 [:scalar :DateTime])))))

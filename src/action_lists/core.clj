;; Copyright (c) Vital Labs, Inc. All rights reserved.  The use and
;; distribution terms for this software are covered by the MIT
;; License (https://opensource.org/licenses/MIT) which can be found
;; in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be
;; bound by the terms of this license.  You must not remove this notice,
;; or any other, from this software.

(ns action-lists.core
  "Clojure port of LispWorks Action Lists.
   Docs: http://www.lispworks.com/documentation/lw70/LW/html/lw-49.htm"
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [com.stuartsierra.dependency :as dep]))

(defonce ^:dynamic *action-lists*
  (ref {}))

(def ^:dynamic *handle-existing-action-list*
  "Contains keywords determining what to do about a given action list
  operation when the action list already exists."
  [:warn :skip])

(def ^:dynamic *handle-existing-action-in-action-list*
  "Contains keywords determining behavior on exceptions raised when an
  action definition already exists in a given action list."
  [:warn :redefine])

(def ^:dynamic *handle-missing-action-list*
  "Defines how to handle an operation on a missing action list."
  :error)

(def ^:dynamic *handle-missing-action-in-action-list*
  "Denotes how to handle an operation on a missing action."
  :warn)

(def ^:dynamic *default-action-list-sort-time*
  "Determines when actions in action lists are sorted."
  :execute)

(declare sorted-actions)

(def ^:dynamic *default-execution-function*
  (fn [action-list args]
    (reduce (fn [retvals action]
              (let [retval (if (and (:once action) (realized? (:once action)))
                             @(:once action)
                             (apply (:data action) args))]
                (when (and (:once action) (not (realized? (:once action))))
                  (deliver (:once action) retval))
                (cond
                  (reduced? retval) retval
                  (nil? retval) retvals
                  :else (conj retvals retval))))
            []
            (sorted-actions action-list))))

(defn make-unregistered-action-list
  "Makes an unregistered action list."
  [{:keys [doc sort-time dummy-actions default-order execution-function]
    :as action-list}]
  (cond-> (assoc action-list
                 :actions (ref {})
                 :sorted-actions (ref [])
                 :graph (ref (dep/graph)))
    (nil? sort-time)
    (assoc :sort-time *default-action-list-sort-time*)
    (nil? execution-function)
    (assoc :execution-function *default-execution-function*)
    (nil? dummy-actions)
    (assoc :dummy-actions #{:start :end})
    (nil? default-order)
    (assoc :default-order {:after [:start] :before [:end]})))

(defn- add-new-action-list
  [action-lists name action-list]
  (dosync
    (commute action-lists
             assoc name
             (make-unregistered-action-list (assoc action-list :name name)))))

(defn- warn
  "Prints a warning message to *err*."
  [key message]
  (println (str "WARNING: " key " " message)))

(defn- redefine-existing-action-list
  [action-lists name action-list]
  (let [[log-op def-op] *handle-existing-action-list*]
    (case log-op
      :warn (warn :redefining-action-list name)
      :silent nil)
    (case def-op
      :skip action-lists
      :redefine (add-new-action-list action-lists name action-list))))

(defn- add-action-list
  [action-lists name action-list]
  (if (contains? @action-lists name)
    (redefine-existing-action-list action-lists name action-list)
    (add-new-action-list action-lists name action-list)))

(defn- find-action-list
  [name-or-list]
  (if (map? name-or-list)
    name-or-list
    (get @*action-lists* name-or-list)))

(defn define-action-list
  "Defines a named action list."
  [name & {:keys [doc sort-time dummy-actions default-order execution-function]
           :as action-list}]
  (add-action-list *action-lists* name action-list))

(defn- handle-missing-action-list
  [name-or-list]
  (case *handle-missing-action-list*
    :error (throw (ex-info "Missing action list."
                           {:missing-action-list name-or-list}))
    :warn (warn :missing-action-list name-or-list)
    :ignore nil))

(defn undefine-action-list
  "Removes the named action list."
  [name]
  (if-let [action-list (find-action-list name)]
    (dosync
      (commute *action-lists* dissoc name))
    (handle-missing-action-list name)))

(defn actions
  "Returns a vector of actions for the action list."
  [name-or-list]
  (if-let [action-list (find-action-list name-or-list)]
    @(:actions action-list)
    (handle-missing-action-list name-or-list)))

(defn- sort-actions
  [action-list]
  (dosync
    (into []
          (comp
            (remove (:dummy-actions action-list))
            (map #(get (ensure (:actions action-list)) %)))
          (dep/topo-sort (ensure (:graph action-list))))))

(defn- sorted-actions
  [action-list]
  (if (= (:sort-time action-list) :execute)
    (sort-actions action-list)
    @(:sorted-actions action-list)))

(defn- get-default-before
  [action-list]
  (get-in action-list [:default-order :before]))

(defn- get-default-after
  [action-list]
  (get-in action-list [:default-order :after]))

(defn- add-new-action
  [action-list action-name data specs]
  (dosync
    (let [action {:name action-name
                  :data data
                  :once (when (:once specs)
                          (promise))
                  :before (:before specs (get-default-before action-list))
                  :after (:after specs (get-default-after action-list))
                  :doc (:doc specs)}]
      (commute (:actions action-list) assoc action-name action)
      (doseq [before (:before action)]
        (commute (:graph action-list) dep/depend before action-name))
      (doseq [after (:after action)]
        (commute (:graph action-list) dep/depend action-name after))
      (ref-set (:sorted-actions action-list) (sort-actions action-list)))))

(defn- redefine-existing-action
  [actions action-name data specs]
  (let [[log-op def-op] *handle-existing-action-in-action-list*]
    (case log-op
      :warn (warn :redefining-action action-name)
      :silent nil)
    (case def-op
      :skip actions
      :redefine (add-new-action actions action-name data specs))))

(defn- add-action
  [action-list action-name data specs]
  (if (and (contains? @(:actions action-list) action-name)
           (not (:force specs)))
    (redefine-existing-action action-list action-name data specs)
    (add-new-action action-list action-name data specs)))

(defn- find-action
  [name-or-list action-name]
  (get (actions name-or-list) action-name))

(defn define-action
  "Adds a new action to a specified list."
  [name-or-list action-name data & {:as specs}]
  (if-let [action-list (find-action-list name-or-list)]
    (add-action action-list action-name data specs)
    (handle-missing-action-list name-or-list)))

(defn undefine-action
  "Removes an action from a specified list."
  [name-or-list action-name]
  (if-let [action-list (find-action-list name-or-list)]
    (dosync
      (let [action (find-action action-list action-name)]
        (commute (:actions (find-action-list name-or-list)) dissoc action-name)
        (commute (:graph action-list) dep/remove-all action-name)
        (ref-set (:sorted-actions action-list) (sort-actions action-list))))
    (handle-missing-action-list name-or-list)))

(defn execute-actions
  "Executes in sequence the actions on a given list."
  [name-or-list & args]
  (if-let [{:keys [execution-function]
            :as action-list} (find-action-list name-or-list)]
    (execution-function action-list args)
    (handle-missing-action-list name-or-list)))

(defn print-actions
  "Prints a listing of the action items on a given action list in order."
  [name-or-list]
  (io!
    (if-let [action-list (find-action-list name-or-list)]
      (doseq [{:keys [name before after]} (sorted-actions action-list)]
        (prn name)
        (doseq [before before]
          (println "        "
                   :before
                   (if (contains? (:dummy-actions action-list) before)
                     :dummy-action
                     :defined-action)
                   (pr-str before)))
        (doseq [after after]
          (println "        "
                   :after
                   (if (contains? (:dummy-actions action-list) after)
                     :dummy-action
                     :defined-action)
                   (pr-str after)))
        (newline))
      (handle-missing-action-list name-or-list))))

(defn print-action-lists
  "Prints a list of all the action lists in the global registry."
  []
  (io!
    (doseq [[name action-list] @*action-lists*]
      (prn name)
      (println "        " (:doc action-list))
      (newline))))

(define-action-list "Clojure startup"
  :doc "Actions to run when Clojure starts up."
  :sort-time :define-action)

(define-action-list "Clojure shutdown"
  :doc "Actions to run when Clojure shuts down."
  :sort-time :define-action)

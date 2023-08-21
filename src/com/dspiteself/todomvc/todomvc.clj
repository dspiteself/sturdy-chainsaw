(ns com.dspiteself.todomvc.todomvc
  (:require [malli.core :as m]))

(def task
  (m/schema
    [:map
     [:text :string]
     ;[:editing :boolean]
     [:completed? :boolean]]))

(def tasks
  (m/schema
    [:vector
     task]))

(def application-state
  (m/schema
    [:map
     [:filter [:enum :all :active :completed]]
     [:editing {:optional true} :int]
     [:tasks tasks]]))

(defn add-task [state text]
  (update state :tasks conj {:text       text
                             :completed? false})
  )
(defn remove-task [state id]
  (update state :tasks (fn [tasks]
                         (-> (into
                               (subvec tasks 0 id)
                               (subvec tasks (inc id)))))))

(defn move-task [state oldpos newpos]
  (update state :tasks (fn [tasks]
                         (let [min-cut (min oldpos newpos)
                               max-cut (max oldpos newpos)]
                           (if (> newpos oldpos)
                             ;move-forward
                             (-> (subvec tasks 0 min-cut)
                                 (into (subvec tasks (inc min-cut) max-cut))
                                 (conj (nth tasks newpos))
                                 (into (subvec tasks (inc max-cut))))
                             (-> (subvec tasks 0 min-cut)
                                 (conj (nth tasks newpos))
                                 (into (subvec tasks (inc min-cut) max-cut))
                                 (into (subvec tasks (inc max-cut)))))))))
(defn edit-task-text [state id text]
  (assoc-in state [:tasks id :text] text))

(defn complete-task! [state id]
  (assoc-in state [:tasks id :completed?] true))

(defn uncomplete-task! [state id]
  (assoc-in state [:tasks id :completed?] false))
(defn select-filter [state f]
  (assoc state :filter f))

(defn init []
  {:filter :all
   :tasks []})


(comment

  )
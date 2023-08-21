(ns com.dspiteself.todomvc.inlinedtodomvc
  (:require [malli.core :as m]))

(def application-state
  (m/schema
    [:map
     [:filter [:enum :all :active :completed]]
     [:task-completed?
      [:vector
       :boolean]]
     [:task-text [:vector
                  :string]]]))

(defn add-task [state text]
  (-> state
      (update :task-completed? conj false)
      (update :task-text conj text))
  )
(defn remove-task [state id]
  (-> state
      (update :task-completed?
              (fn [x]
                (-> (into
                      (subvec x 0 id)
                      (subvec x (inc id))))))
      (update :task-text
              (fn [x]
                (-> (into
                      (subvec x 0 id)
                      (subvec x (inc id))))))))

(defn move-task [state oldpos newpos]
  (let [min-cut (min oldpos newpos)
        max-cut (max oldpos newpos)
        move-forward (> newpos oldpos)]
    (-> state
        (update :task-completed?
                (fn [x]
                  (if move-forward
                    ;move-forward
                    (-> (subvec x 0 min-cut)
                        (into (subvec x (inc min-cut) max-cut))
                        (conj (nth x newpos))
                        (into (subvec x (inc max-cut))))
                    (-> (subvec x 0 min-cut)
                        (conj (nth x newpos))
                        (into (subvec x (inc min-cut) max-cut))
                        (into (subvec x (inc max-cut)))))))
        (update :task-text
                (fn [x]
                  (if move-forward
                    ;move-forward
                    (-> (subvec x 0 min-cut)
                        (into (subvec x (inc min-cut) max-cut))
                        (conj (nth x newpos))
                        (into (subvec x (inc max-cut))))
                    (-> (subvec x 0 min-cut)
                        (conj (nth x newpos))
                        (into (subvec x (inc min-cut) max-cut))
                        (into (subvec x (inc max-cut))))))))))
(defn edit-task-text [state id text]
  (assoc-in state [:task-text id] text))

(defn complete-task! [state id]
  (assoc-in state [:task-completed? id] true))

(defn uncomplete-task! [state id]
  (assoc-in state [:task-completed? id] false))
(defn select-filter [state f]
  (assoc state :filter f))

(defn init []
  {:filter :all
   :task-completed? []
   :task-text []})
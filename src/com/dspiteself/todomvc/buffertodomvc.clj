(ns com.dspiteself.todomvc.buffertodomvc
  (:require [malli.core :as m])
  (:import (io.netty5.buffer Buffer BufferAllocator)
           (java.nio.charset Charset)))
(set! *warn-on-reflection* true)
(def application-state
  (m/schema
    [:map
     [:filter [:enum :all :active :completed]]
     [:task-completed?
      [:vector
       :boolean]]
     [:task-text [:vector
                  :string]]]))

(def ^Charset utf (Charset/forName "UTF-8"))
(defn add-task [state ^String text]
  (let [task-completed? ^Buffer (:task-completed? state)
        task-text-starts ^Buffer (:task-text-starts state)
        task-text-data ^Buffer (:task-text-data state)]
    (.writeBoolean task-completed? false)
    (.writeCharSequence task-text-data text utf)
    (.writeInt task-text-starts (.writerOffset task-text-data))
    state))
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

(defn reset-state [state]
  (let [task-completed? ^Buffer (:task-completed? state)
        task-text-starts ^Buffer (:task-text-starts state)
        task-text-data ^Buffer (:task-text-data state)]
    (.writerOffset task-completed? 0)
    (.writerOffset task-text-starts 0)
    (.writerOffset task-text-data 0)
    (.readerOffset task-completed? 0)
    (.readerOffset task-text-starts 0)
    (.readerOffset task-text-data 0)
    state))

(def ^BufferAllocator default-allocator (BufferAllocator/onHeapPooled ))
(defn init []
  {:filter :all
   :task-completed? (.allocate default-allocator 100)
   :task-text-starts (.allocate default-allocator 100)
   :task-text-data (.allocate default-allocator 1000)})
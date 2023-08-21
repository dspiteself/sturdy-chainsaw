(ns com.dspiteself.sturdy-chainsaw
  (:require [malli.core :as m]
            [criterium.core :as cc]
            [com.dspiteself.todomvc.todomvc :as todomvc-baseline]
            [com.dspiteself.todomvc.inlinedtodomvc :as todomvc-inline]
            [com.dspiteself.todomvc.splitposvectodomvc :as todomvc-splitpos]
            [com.dspiteself.todomvc.buffertodomvc :as buffertodomvc]))


(defn add-tasks [state f]
  (reduce
    f
    state
    (repeat 1000 "text")))




(comment
  (let [init (todomvc-baseline/init)]
    (cc/quick-bench (add-tasks
                      init
                      todomvc-baseline/add-task) ))

  (let [init (todomvc-inline/init)]
    (cc/quick-bench (add-tasks
                      init
                      todomvc-inline/add-task)))
  (let [init (todomvc-splitpos/init)]
    (cc/quick-bench (add-tasks
                      init
                      todomvc-splitpos/add-task)))
  (let [init (buffertodomvc/init)]
    (cc/quick-bench (add-tasks
                      (buffertodomvc/reset-state init)
                      buffertodomvc/add-task)))

  )

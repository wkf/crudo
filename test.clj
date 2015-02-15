

  ;; [db task]
  ;;
  ;; (let [e (tempid)
  ;;       a-fns {:type #([:db/add e :task/type %])
  ;;              :state #([:db/add e :task/state %])
  ;;              :repo #([:db/add e :task/repo %])
  ;;              :commit #([:db/add e :task/commit %])
  ;;              :params (partial
  ;;                        mapcat
  ;;                        (fn [[k v]]
  ;;                          [{:db/id (tempid)
  ;;                            :task/_params e
  ;;                            :task.params/-key k
  ;;                            :task.params/-value v}]))}]
  ;;
  ;;   (mapcat
  ;;     (fn [[k v]]
  ;;       (if-let [a-fn (k a-fns)] (a-fn v) [])) task))
  ;;
  ;;
  ;; #FIXME:
  ;;  Q: but sir, you are confusing your wire schema with your domain schema with your database schema
  ;;  A: yes, yes I am
  ;;

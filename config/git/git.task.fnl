(local hm (require :heime))
(local utils (require :heime.utils))

(let [run (fn [ctx data]
            (ctx:copy_template :gitconfig.tmpl (ctx.user:home_file :.gitconfig)))]
  (hm.task {:name :git :description "setup and configure node" : run}))

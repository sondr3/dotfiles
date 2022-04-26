(let [hm (require :heime)
      utils (require :heime.utils)
      run (fn [ctx data]
            (ctx:copy_template :gitconfig.tmpl (ctx.user:home_file :.gitconfig)))]
  (hm.task {:name :git :description "setup and configure node" : run}))

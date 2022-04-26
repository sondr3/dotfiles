(let [hm (require :heime)
      utils (require :heime.utils)
      run (fn [ctx data]
            (ctx:copy_file :config.toml
                           (ctx.user:config_file :cargo :config.toml)))]
  (hm.task {:name :rust :description "setup and configure rust" : run}))

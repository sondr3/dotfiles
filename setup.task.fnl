(let [hm (require :heime)
      utils (require :heime.utils)
      setup (fn [ctx]
              (ctx:copy_file :config.lua (ctx:config_file :heime :config.lua)))]
  (hm.setup {: setup}))
(local hm (require :heime))
(local utils (require :heime.utils))

(let [setup (fn [ctx]
              (ctx:copy_file :config.lua
                             (ctx.user:config_file :heime :config.lua)))]
  (hm.setup {: setup}))

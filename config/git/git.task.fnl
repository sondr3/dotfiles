(local hm (require :heime))
(local utils (require :heime.utils))

(hm.task {:name :git
          :description "setup and configure node"
          :run (fn [ctx data]
                 (ctx:copy_template :gitconfig.tmpl
                                    (ctx.user:home_file :.gitconfig)))})

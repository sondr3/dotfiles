(let [hm (require :heime)
      utils (require :heime.utils)
      run (fn [ctx data]
            (ctx:copy_directory :nvim (ctx:config_file :nvim))
            (ctx:copy_file :stylua.toml (ctx:config_file :nvim :stylua.toml))
            (ctx:write_string (ctx:config_file :nvim :.styluaignore)
                              :lua/packer_compiled.lua))]
  (hm.task {:name :neovim :description "setup and configure neovim" : run}))

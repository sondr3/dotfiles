[{1 :andersevenrud/nordic.nvim
    :priority 1000
  :lazy false
  :init (fn [] (vim.cmd.colorscheme :nordic))}
 {1 :Olical/conjure
  :config (fn [_ opts]
            ((. (require :conjure.main) :main))
            ((. (require :conjure.mapping) :on-filetype)))
  :dependencies [{1 :PaterJason/cmp-conjure
                  :config (fn []
                            (local cmp (require :cmp))
                            (local config (cmp.get_config))
                            (table.insert config.sources
                                          {:name :buffer
                                           :option {:sources [{:name :conjure}]}})
                            (cmp.setup config))}]
  :ft [:clojure :fennel]
  :init (fn [] (tset vim.g "conjure#debug" true))}]

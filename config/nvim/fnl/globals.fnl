;; configure leaders
(do 
  (vim.keymap.set [:n :v] :<Space> :<Nop> {:silent true})
  (set vim.g.mapleader " ")
  (set vim.g.maplocalleader "\\"))

(let [lazy-path (.. (vim.fn.stdpath :data) :/lazy/lazy.nvim)]
  (when (not ((. (or vim.uv vim.loop) :fs_stat) lazy-path))
    (vim.fn.system [:git
                    :clone
                    "--filter=blob:none"
                    "https://github.com/folke/lazy.nvim.git"
                    :--branch=stable
                    lazy-path]))
  (vim.opt.runtimepath:prepend lazy-path))

(require :globals)
(require :settings)

(let [lazy (require :lazy)]
  (lazy.setup :plugins
              {:checker {:enabled false}
               :defaults {:lazy true}
               :install {:colorscheme [:nordic]}
               :performance {:rtp {:reset false} :reset_packpath false}}))

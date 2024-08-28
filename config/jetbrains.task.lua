local hm = require("heime")
local utils = require("heime.utils")

local template = [[ 
set surround
set hlsearch
set incsearch
set ignorecase
set smartcase
set incsearch
set showmode
set number
set relativenumber

let mapleader = " "

imap jk <esc>

nmap <leader>r :action Run<cr>
nmap <leader>t :action Refactorings.QuickListPopupAction<cr>
nmap <leader>b :action CompileDirty<cr>
set ideajoin
]]

return hm.task({
  name = "jetbrains",
  description = "setup and configure jetbrains",
  run = function(ctx)
    ctx:write(hm.path(hm.home_dir, ".ideavimrc"), utils:template(template, {}))
  end,
})

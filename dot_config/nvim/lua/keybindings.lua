vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local lua_leader_map = require("utils").lua_leader_map
local leader_map = require("utils").leader_map

lua_leader_map("<space>", "require('telescope.builtin').buffers()")
lua_leader_map("bb", "require('telescope.builtin').buffers()")

lua_leader_map("ff", "require('telescope.builtin').find_files()")
lua_leader_map("fb", "require('telescope.builtin').file_browser()")
lua_leader_map("fg", "require('telescope.builtin').live_grep()")
lua_leader_map("fs", "require('telescope.builtin').grep_string()")

lua_leader_map("gs", "require('telescope.builtin').git_status()")
lua_leader_map("gb", "require('telescope.builtin').git_branches()")
lua_leader_map("gc", "require('telescope.builtin').git_commits()")

leader_map("t", "ToggleTerm")
leader_map("T", "ToggleTerm direction='float'")

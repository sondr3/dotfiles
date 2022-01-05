local wk = require("which-key")

vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local lua_cmd = function(cmd)
  return "<cmd>lua " .. cmd .. "<CR>"
end

local cmd = function(cmd)
  return "<cmd> " .. cmd .. "<CR>"
end

wk.register({
  ["<leader>"] = {
    ["<space>"] = { lua_cmd("require('telescope.builtin').buffers()"), "buffers" },
    b = {
      name = "+buffer",
      b = { lua_cmd("require('telescope.builtin').buffers()"), "buffers" },
    },
    f = {
      name = "+file",
      f = { lua_cmd("require('telescope.builtin').find_files()"), "find files" },
      b = { lua_cmd("require('telescope.builtin').file_browser()"), "file browser" },
      g = { lua_cmd("require('telescope.builtin').live_grep()"), "live grep" },
      s = { lua_cmd("require('telescope.builtin').grep_string()"), "grep string" },
    },
    g = {
      name = "+git",
      s = { lua_cmd("require('telescope.builtin').git_status()"), "git status" },
      b = { lua_cmd("require('telescope.builtin').git_branches()"), "git branches" },
      c = { lua_cmd("require('telescope.builtin').git_commits()"), "git commits" },
    },
    t = { cmd("ToggleTerm direction='float'"), "floating terminal" },
    T = { cmd("ToggleTerm"), "bottom terminal" },
  },
})
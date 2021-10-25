local M = {}

local key_opts = { noremap = true, silent = true }

function M.lua_leader_map(key, cmd)
  vim.api.nvim_set_keymap("n", "<leader>" .. key, "<cmd>lua " .. cmd .. "<CR>", key_opts)
end

function M.leader_map(key, cmd)
  vim.api.nvim_set_keymap("n", "<leader>" .. key, "<cmd> " .. cmd .. "<CR>", key_opts)
end

return M

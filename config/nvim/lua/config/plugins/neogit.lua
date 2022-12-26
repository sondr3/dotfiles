local M = {
  "TimUntersberger/neogit",
  cmd = "Neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
  },
}

M.config = function()
  require("neogit").setup({
    disable_commit_confirmation = true,
    disable_insert_on_commit = false,
    integrations = {
      diffview = true,
    },
  })
end

M.init = function()
  vim.keymap.set("n", "<leader>gg", "<cmd>Neogit kind=floating<cr>", { desc = "Neogit" })
  vim.keymap.set("n", "<leader>gs", function()
    require("telescope.builtin").git_status()
  end, { desc = "git status" })
  vim.keymap.set("n", "<leader>gs", function()
    require("telescope.builtin").git_branches()
  end, { desc = "git branches" })
  vim.keymap.set("n", "<leader>gs", function()
    require("telescope.builtin").git_commits()
  end, { desc = "git commits" })
end

return M

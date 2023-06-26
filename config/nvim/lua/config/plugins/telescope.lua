local M = {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    { "nvim-lua/popup.nvim" },
    { "nvim-lua/plenary.nvim" },
    { "nvim-telescope/telescope-file-browser.nvim" },
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
    },
    {
      "nvim-telescope/telescope-frecency.nvim",
      requires = { "kkharji/sqlite.lua" },
    },
  },
}

M.project_files = function()
  local builtin = require("telescope.builtin")
  local in_git_repo = vim.fn.systemlist("git rev-parse --is-inside-work-tree")[1] == "true"

  local opts = {
    follow = true,
    hidden = true,
    show_untracked = true,
    use_git_root = false,
  }

  if in_git_repo then
    builtin.git_files(opts)
  else
    builtin.find_files(opts)
  end
end

M.config = function()
  require("telescope").load_extension("file_browser")
  require("telescope").load_extension("fzf")
  require("telescope").load_extension("frecency")

  local telescope = require("telescope")
  local actions = require("telescope.actions")
  local trouble = require("trouble.providers.telescope")
  local frecency = require("utils.frecency")

  telescope.setup({
    defaults = {
      initial_mode = "insert",
      -- file_sorter = frecency.frecency_sorter,
      mappings = {
        i = {
          ["<esc>"] = actions.close, -- escape closes popup
          ["<c-t>"] = trouble.open_with_trouble,
        },
        n = {
          ["<c-t>"] = trouble.open_with_trouble,
        },
      },
    },
    extensions = {
      fzf = {
        fuzzy = true, -- false will only do exact matching
        override_generic_sorter = true, -- override the generic sorter
        override_file_sorter = true, -- override the file sorter
        case_mode = "smart_case", -- or "ignore_case" or "respect_case", the default case_mode is "smart_case"
      },
      file_browser = {
        theme = "ivy",
        -- disables netrw and use telescope-file-browser in its place
        hijack_netrw = true,
      },
    },
  })
end

M.init = function()
  vim.keymap.set("n", "<leader><space>", function()
    require("telescope.builtin").buffers()
  end)
  vim.keymap.set("n", "<leader>ff", function()
    require("config.plugins.telescope").project_files()
  end, { noremap = true })
  vim.keymap.set("n", "<leader>fb", function()
    require("telescope").extensions.file_browser.file_browser()
  end)
  vim.keymap.set("n", "<leader>fg", function()
    require("telescope.builtin").live_grep()
  end)
  vim.keymap.set("n", "<leader>fs", function()
    require("telescope.builtin").grep_string()
  end)
end

return M

local telescope = require("telescope")
local actions = require("telescope.actions")
local trouble = require("trouble.providers.telescope")

local M = {}

telescope.setup({
	defaults = {
		initial_mode = "insert",
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
	},
})

telescope.load_extension("fzf")
telescope.load_extension("file_browser")

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

return M

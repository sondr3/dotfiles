local present, which_key = pcall(require, "which-key")
if not present then
	return
end

-- Set space as the leader key
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Remap g/j to deal with word wrap better
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

local lua_cmd = function(cmd)
	return "<cmd>lua " .. cmd .. "<CR>"
end

local cmd = function(cmd)
	return "<cmd> " .. cmd .. "<CR>"
end

which_key.setup({})

which_key.register({
	["<leader>"] = {
		["<space>"] = { lua_cmd("require('telescope.builtin').buffers()"), "buffers" },
		b = {
			name = "+buffer",
			b = { lua_cmd("require('telescope.builtin').buffers()"), "buffers" },
		},
		f = {
			name = "+file",
			f = { lua_cmd("require('config/telescope').project_files()"), "find files" },
			b = { cmd(":Telescope file_browser"), "file browser" },
			g = { lua_cmd("require('telescope.builtin').live_grep()"), "live grep" },
			s = { lua_cmd("require('telescope.builtin').grep_string()"), "grep string" },
		},
		G = {
			name = "+git",
			g = { lua_cmd("require('neogit').open()"), "neogit" },
			s = { lua_cmd("require('telescope.builtin').git_status()"), "git status" },
			b = { lua_cmd("require('telescope.builtin').git_branches()"), "git branches" },
			c = { lua_cmd("require('telescope.builtin').git_commits()"), "git commits" },
		},
		t = {
			name = "+terminal",
			t = { cmd("ToggleTerm direction='float'"), "floating terminal" },
			v = { cmd("ToggleTerm direction='vertical'"), "vertical terminal" },
			b = { cmd("ToggleTerm direction='horizontal'"), "bottom terminal" },
		},
	},
})

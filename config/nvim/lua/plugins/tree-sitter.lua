vim.pack.add({
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects", version = "main" },
})

vim.api.nvim_create_autocmd("PackChanged", {
	desc = "Update tree-sitter",
	group = vim.api.nvim_create_augroup("pack-update-tree-sitter", { clear = true }),
	callback = function(event)
		if event.data.kind == "update" then
			vim.cmd("TSUpdate")
		end
	end,
})

-- ensure basic parser are installed
local parsers = { "stable" }
require("nvim-treesitter").install(parsers)

---@param buf integer
---@param language string
local function treesitter_attach(buf, language)
	-- check if parser exists before starting highlighter
	if not vim.treesitter.language.add(language) then
		return
	end
	-- enables syntax highlighting and other treesitter features
	vim.treesitter.start(buf, language)

	-- enables treesitter based folds
	vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
	vim.wo.foldmethod = "manual"

	-- enables treesitter based indentation
	vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
end

local available_parsers = require("nvim-treesitter.config").get_available()
vim.api.nvim_create_autocmd("FileType", {
	callback = function(args)
		local buf, filetype = args.buf, args.match
		local language = vim.treesitter.language.get_lang(filetype)
		if not language then
			return
		end

		if not (treesitter_attach(buf, language) or vim.tbl_contains(available_parsers, language)) then
			return
		end

		-- automaically install parser for missing languages
		-- attempt to install even if it is available accoring to `vim.treesitter.langauge.add()`,
		-- to ensure the latest version is installed using `nvim-treesitter`, instead of the outdated vendored parser
		if
			not vim.tbl_contains(require("nvim-treesitter.config").get_installed("parsers"), language)
		then
			-- attempt to start highlighter after installing missing language
			require("nvim-treesitter.install")
				.install(language)
				:await(function() treesitter_attach(buf, language) end)
		end
	end,
})

require("nvim-treesitter-textobjects").setup({})

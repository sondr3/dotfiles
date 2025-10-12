-- Highlight when yanking (copying) text
vim.api.nvim_create_autocmd("TextYankPost", {
	desc = "Highlight when yanking (copying) text",
	group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
	callback = function() vim.hl.on_yank({ timeout = 200 }) end,
})

-- Restore cursor position on file open
vim.api.nvim_create_autocmd("BufReadPost", {
	desc = "Restore cursor position on file open",
	group = vim.api.nvim_create_augroup("restore-cursor", { clear = true }),
	pattern = "*",
	callback = function()
		local line = vim.fn.line("'\"")
		if line > 1 and line <= vim.fn.line("$") then
			vim.cmd("normal! g'\"")
		end
	end,
})

-- auto-create missing dirs when saving a file
vim.api.nvim_create_autocmd("BufWritePre", {
	desc = "Auto-create missing dirs when saving a file",
	group = vim.api.nvim_create_augroup("auto-create-dir", { clear = true }),
	pattern = "*",
	callback = function()
		local dir = vim.fn.expand("<afile>:p:h")
		if vim.fn.isdirectory(dir) == 0 then
			vim.fn.mkdir(dir, "p")
		end
	end,
})

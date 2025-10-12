vim.pack.add({
	"https://github.com/saghen/blink.cmp",
	"https://github.com/rafamadriz/friendly-snippets",
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/xzbdmw/colorful-menu.nvim",
	"https://github.com/folke/lazydev.nvim",
	"https://github.com/nvim-mini/mini.nvim",
	"https://github.com/onsails/lspkind.nvim",
})

require("lazydev").setup({
	library = {
		-- Load luvit types when the `vim.uv` word is found
		{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
	},
})

require("lspkind").init({
	preset = "default",
})
require("mini.icons").setup({})

require("blink.cmp").setup({
	keymap = {
		preset = "super-tab",
		["<CR>"] = { "accept", "fallback" },
	},
	appearance = {
		nerd_font_variant = "mono",
	},
	completion = {
		menu = {
			draw = {
				-- We don't need label_description now because label and label_description are already
				-- combined together in label by colorful-menu.nvim.
				columns = { { "kind_icon" }, { "label", gap = 1 } },
				components = {
					label = {
						text = function(ctx) return require("colorful-menu").blink_components_text(ctx) end,
						highlight = function(ctx)
							return require("colorful-menu").blink_components_highlight(ctx)
						end,
					},
					kind_icon = {
						text = function(ctx)
							if vim.tbl_contains({ "Path" }, ctx.source_name) then
								local mini_icon, _ = require("mini.icons").get_icon(ctx.item.data.type, ctx.label)
								if mini_icon then
									return mini_icon .. ctx.icon_gap
								end
							end

							local icon = require("lspkind").symbolic(ctx.kind, { mode = "symbol" })
							return icon .. ctx.icon_gap
						end,

						-- Optionally, use the highlight groups from mini.icons
						-- You can also add the same function for `kind.highlight` if you want to
						-- keep the highlight groups in sync with the icons.
						highlight = function(ctx)
							if vim.tbl_contains({ "Path" }, ctx.source_name) then
								local mini_icon, mini_hl =
									require("mini.icons").get_icon(ctx.item.data.type, ctx.label)
								if mini_icon then
									return mini_hl
								end
							end
							return ctx.kind_hl
						end,
					},
					kind = {
						-- Optional, use highlights from mini.icons
						highlight = function(ctx)
							if vim.tbl_contains({ "Path" }, ctx.source_name) then
								local mini_icon, mini_hl =
									require("mini.icons").get_icon(ctx.item.data.type, ctx.label)
								if mini_icon then
									return mini_hl
								end
							end
							return ctx.kind_hl
						end,
					},
				},
			},
		},
		-- By default, you may press `<c-space>` to show the documentation.
		-- Optionally, set `auto_show = true` to show the documentation after a delay.
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		ghost_text = { enabled = true },
	},
	sources = {
		default = { "lsp", "path", "snippets", "buffer", "lazydev" },
		providers = {
			lazydev = { module = "lazydev.integrations.blink", score_offset = 100 },
		},
	},
	snippets = { preset = "default" },
	signature = { enabled = true },
	fuzzy = { implementation = "prefer_rust", prebuilt_binaries = { force_version = "v1.7.0" } },
})

vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
require("conform").setup({
	notify_on_error = false,
	format_on_save = {
		timeout_ms = 500,
		lsp_format = "fallback",
	},
	formatters_by_ft = {
		lua = { "stylua" },
	},
})

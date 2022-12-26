local M = {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    { "neovim/nvim-lspconfig" },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-nvim-lua" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-path" },
    { "hrsh7th/cmp-cmdline" },
    { "onsails/lspkind-nvim" },
    { "saadparwaiz1/cmp_luasnip" },
  },
}

M.config = function()
  local cmp = require("cmp")
  local luasnip = require("luasnip")
  local lsp_kind = require("lspkind")

  cmp.setup({
    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },
    formatting = {
      format = lsp_kind.cmp_format({}),
    },
    mapping = cmp.mapping.preset.insert({
      ["<C-Space>"] = cmp.mapping.complete(),
      ["<Down>"] = cmp.mapping.select_next_item({
        behavior = cmp.SelectBehavior.Select,
      }),
      ["<Up>"] = cmp.mapping.select_prev_item({
        behavior = cmp.SelectBehavior.Select,
      }),
      ["<C-b>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),
      ["<C-y>"] = cmp.config.disable,
      ["<C-e>"] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ["<CR>"] = cmp.mapping.confirm({ select = true }),
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          local entry = cmp.get_selected_entry()
          if not entry then
            cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
          else
            cmp.confirm()
          end
        elseif luasnip.expand_or_locally_jumpable() then
          vim.fn.feedkeys(
            vim.api.nvim_replace_termcodes("<Plug>luasnip-expand-or-jump", true, true, true),
            ""
          )
        else
          fallback()
        end
      end, {
        "i",
        "s",
        "c",
      }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          vim.fn.feedkeys(
            vim.api.nvim_replace_termcodes("<Plug>luasnip-jump-prev", true, true, true),
            ""
          )
        else
          fallback()
        end
      end, {
        "i",
        "s",
        "c",
      }),
    }),
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "luasnip", max_item_count = 4 },
      { name = "path" },
      { name = "nvim_lua" },
    }, {
      { name = "buffer", options = { keyword_length = 5 } },
    }),
    experimental = {
      ghost_text = true,
    },
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ "/", "?" }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = "buffer" },
    },
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(":", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })
end

return M

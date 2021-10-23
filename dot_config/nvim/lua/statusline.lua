local colors = require("nordbuddy.palette")
local lsp = require("feline.providers.lsp")
local vi_mode_utils = require("feline.providers.vi_mode")
local git_utils = require("feline.providers.git")

local function git_type_exists(gsd, type)
  return gsd[type] and gsd[type] > 0
end

local function git_changes()
  local gsd = vim.b.gitsigns_status_dict

  if gsd then
    return git_type_exists(gsd, "added") or git_type_exists(gsd, "changed") or git_type_exists(gsd, "removed")
  end

  return false
end

local vi_mode_colors = {
  NORMAL = colors.gray,
  INSERT = colors.blue,
  VISUAL = colors.purple,
  OP = colors.green,
  BLOCK = colors.purple,
  REPLACE = colors.red,
  ["V-REPLACE"] = colors.red,
  ENTER = colors.intense_blue,
  MORE = colors.purple,
  SELECT = colors.orange,
  COMMAND = colors.purple,
  SHELL = colors.purple,
  TERM = colors.intense_blue,
  NONE = colors.white,
}

local comps = {
  vi_mode = {
    provider = "vi_mode",
    hl = function()
      return {
        bg = colors.dark_white,
        fg = vi_mode_utils.get_mode_color(),
        name = vi_mode_utils.get_mode_highlight_name(),
        style = "bold",
      }
    end,
    left_sep = { " ", "left_rounded" },
    right_sep = "block",
    icon = "",
  },
  file = {
    info = {
      provider = "file_info",
      hl = { bg = colors.gray },
      left_sep = "block",
    },
    type = {
      provider = "file_type",
      hl = { bg = colors.gray },
      left_sep = "block",
      right_sep = "block",
    },
    encoding = {
      provider = "file_encoding",
      hl = { bg = colors.gray },
      left_sep = "block",
      right_sep = "block",
    },
  },
  git = {
    branch = {
      provider = "git_branch",
      icon = " ",
      hl = { style = "bold", bg = colors.gray },
      left_sep = "block",
      right_sep = "block",
    },
    added = {
      provider = "git_diff_added",
      hl = { fg = colors.green, bg = colors.grayish },
      right_sep = "block",
      icon = " +",
    },
    changed = {
      provider = "git_diff_changed",
      hl = { fg = colors.intense_blue, bg = colors.grayish },
      right_sep = "block",
      icon = " ~",
    },
    removed = {
      provider = "git_diff_removed",
      hl = { fg = colors.red, bg = colors.grayish },
      icon = " -",
    },
  },
  diagnostics = {
    error = {
      provider = "diagnostics_error",
      hl = { fg = colors.red },
      enabled = lsp.diagnostics_exist("Error"),
    },
    warning = {},
    info = {},
    hint = {},
  },
  lsp = {
    name = {
      provider = "lsp_client_names",
      hl = { fg = colors.white },
      enabled = lsp.is_lsp_attached(),
    },
  },
  position = {
    provider = "position",
    left_sep = "block",
    right_sep = { "right_rounded", " " },
    hl = { fg = colors.gray, style = "bold", bg = colors.dark_white },
  },
}

local components = {
  active = {
    {
      comps.vi_mode,
      comps.file.info,
      comps.git.branch,
      comps.git.added,
      comps.git.changed,
      comps.git.removed,
      {
        provider = " ",
        hl = function()
          if git_changes() then
            return { bg = colors.grayish }
          else
            return { bg = colors.gray }
          end
        end,
        right_sep = "right_rounded",
        always_visible = true,
      },
    },
    {
      comps.lsp.name,
      comps.diagnostics.error,
      comps.diagnostics.warning,
      comps.diagnostics.info,
      comps.diagnostics.hint,
    },
    {
      {
        provider = " ",
        hl = { bg = colors.gray },
        left_sep = "left_rounded",
        always_visible = true,
      },
      comps.file.encoding,
      comps.file.type,
      comps.position,
    },
  },
  inactive = {
    {},
    {},
  },
}

require("feline").setup({
  colors = {
    fg = colors.bright_cyan,
    bg = colors.dark_black,
  },
  vi_mode_colors = vi_mode_colors,
  components = components,
})

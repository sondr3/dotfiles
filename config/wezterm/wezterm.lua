local wezterm = require("wezterm")

local is_macos = function()
  return wezterm.target_triple == "aarch64-apple-darwin"
end

local get_key_modifier = function()
  if is_macos() then
    return "CMD"
  else
    return "SHIFT|CTRL"
  end
end

local get_font_size = function()
  if is_macos() then
    return 16
  else
    return 14
  end
end

local key_mods = get_key_modifier()

return {
  check_for_updates = false,
  color_scheme = "nord",
  font = wezterm.font("PragmataPro Mono Liga"),
  enable_wayland = true,
  font_size = get_font_size(),
  hide_tab_bar_if_only_one_tab = true,
  keys = {
    {
      key = "t",
      mods = key_mods,
      action = wezterm.action({
        SpawnCommandInNewTab = {
          cwd = "$HOME",
        },
      }),
    },
  },
}

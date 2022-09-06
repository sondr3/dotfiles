local wezterm = require("wezterm")

local get_key_modifier = function()
  if wezterm.target_triple == "aarch64-apple-darwin" then
    return "CMD"
  else
    return "SHIFT|CTRL"
  end
end

local key_mods = get_key_modifier()

return {
  check_for_updates = false,
  color_scheme = "nord",
  font = wezterm.font("PragmataPro Mono Liga"),
  enable_wayland = true,
  font_size = 14,
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

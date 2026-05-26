local wezterm = require("wezterm")
local config = wezterm.config_builder()

local is_macos = function() return wezterm.target_triple == "aarch64-apple-darwin" end

local set_on_macos = function(mac_value, linux_value)
	if is_macos() then
		return mac_value
	else
		return linux_value
	end
end

local get_key_modifier = function() return set_on_macos("CMD", "SHIFT|CTRL") end

local get_font_size = function() return set_on_macos(16, 14) end

local key_mods = get_key_modifier()

config.check_for_updates = false
config.color_scheme = "nord"
config.font = wezterm.font("PragmataPro Mono Liga")
config.enable_wayland = true
config.enable_kitty_keyboard = true
config.font_size = get_font_size()
config.hide_tab_bar_if_only_one_tab = true
config.keys = {
	{
		key = "t",
		mods = key_mods,
		action = wezterm.action({
			SpawnCommandInNewTab = {
				cwd = "$HOME",
			},
		}),
	},
}

return config

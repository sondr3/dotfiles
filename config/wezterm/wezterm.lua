local wezterm = require("wezterm")

local is_macos = function()
	return wezterm.target_triple == "aarch64-apple-darwin"
end

local set_on_macos = function(mac_value, linux_value)
	if is_macos() then
		return mac_value
	else
		return linux_value
	end
end

local get_key_modifier = function()
	return set_on_macos("CMD", "SHIFT|CTRL")
end

local get_font_size = function()
	return set_on_macos(16, 14)
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

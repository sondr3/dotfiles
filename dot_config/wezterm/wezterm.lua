local wezterm = require("wezterm")

return {
	color_scheme = "nord",
	font = wezterm.font("PragmataPro Mono Liga"),
	enable_wayland = true,
	font_size = 14,
	hide_tab_bar_if_only_one_tab = true,
	keys = {
		{
			key = "t",
			mods = "SHIFT|CTRL",
			action = wezterm.action({
				SpawnCommandInNewTab = {
					cwd = "$HOME",
				},
			}),
		},
	},
}

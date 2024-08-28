local hm = require("heime")

return hm.task({
  name = "wezterm",
  description = "setup and configure wezterm",
  run = function(ctx)
    ctx:copy("wezterm.lua", hm.path(hm.config_dir, "wezterm", "wezterm.lua"))
  end,
})

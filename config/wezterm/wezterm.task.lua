local hm = require("heime")

return hm.task({
  name = "wezterm",
  description = "setup and configure wezterm",
  run = function(ctx)
    ctx:copy_file("wezterm.lua", ctx:config_file("wezterm", "wezterm.lua"))
  end,
})

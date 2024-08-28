local hm = require("heime")

local path = function()
  if hm.is_linux then
    return hm.path(hm.config_dir, "Code", "User", "settings.json")
  elseif hm.is_macos then
    return hm.path(hm.home_dir, "Library", "Application Support", "Code", "User", "settings.json")
  end
end

return hm.task({
  name = "vscode",
  description = "setup and configure vscode",
  run = function(ctx)
    ctx:copy("settings.json", path())
  end,
})

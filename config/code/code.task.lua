local hm = require("heime")

local path = function(ctx)
  if ctx.is_linux() then
    return ctx:config_file("Code", "User", "settings.json")
  elseif ctx.is_macos() then
    return ctx:home_file("Library", "Application Support", "Code", "User", "settings.json")
  end
end

return hm.task({
  name = "vscode",
  description = "setup and configure vscode",
  run = function(ctx)
    ctx:copy_file("settings.json", path(ctx))
  end,
})

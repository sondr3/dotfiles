local hm = require("heime")

return hm.task({
  name = "vscode",
  description = "setup and configure vscode",
  run = function(ctx)
    ctx:copy_file(
      "settings.json",
      ctx:home_file("Library", "Application Support", "Code", "User", "settings.json")
    )
  end,
})

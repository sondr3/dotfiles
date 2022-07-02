local hm = require("heime")

return hm.task({
  name = "git",
  description = "setup and configure node",
  run = function(ctx)
    ctx:copy_template("gitconfig.tmpl", ctx:home_file(".gitconfig"))
  end,
})

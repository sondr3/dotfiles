local hm = require("heime")

return hm.task({
  name = "git",
  description = "setup and configure node",
  run = function(ctx)
    ctx:template("gitconfig.tmpl", hm.path(hm.home_dir, ".gitconfig"))

    ctx:copy("gitignore-global", hm.path(hm.home_dir, ".gitignore"))
  end,
})

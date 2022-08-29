local hm = require("heime")

return hm.task({
  name = "git",
  description = "setup and configure node",
  run = function(ctx)
    ctx:copy_template("gitconfig.tmpl", ctx:home_file(".gitconfig"))

    ctx:copy_file("gitignore-global", ctx:home_file(".gitignore"))
    ctx:copy_template("gitconfig-eviny", ctx:home_file(".gitconfig-eviny"))
    ctx:copy_template("gitconfig-sonat", ctx:home_file(".gitconfig-sonat"))
  end,
})

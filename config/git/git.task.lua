local hm = require("heime")

return hm.task({
  name = "git",
  description = "setup and configure node",
  run = function(ctx)
    -- ctx:install({
    --   ["rust-parallel"] = {
    --     windows = "cargo",
    --     macos = "homebrew"
    --   };
    --   "git";
    --   "fzf";
    -- })
    ctx:template("gitconfig.tmpl", hm.path(hm.home_dir, ".gitconfig"))

    ctx:copy("gitignore-global", hm.path(hm.home_dir, ".gitignore"))
  end,
})

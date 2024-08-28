local hm = require("heime")

local agent = [=[
# Allow all files from my account
[[ssh-keys]]
account = "my.1password.com"
]=]

return hm.task({
  name = "1password",
  description = "setup and configure 1Password",
  run = function(ctx)
    ctx:write(hm.path(hm.config_dir, "1Password", "ssh", "agent.toml"), agent)
  end,
})

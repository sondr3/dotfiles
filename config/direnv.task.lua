local hm = require("heime")

local config = [[ 
[global]
load_dotenv = true
]]

return hm.task({
  name = "direnv",
  description = "setup and configure direnv",
  run = function(ctx)
    ctx:write(hm.path(hm.config_dir, "direnv", "direnv.toml"), config)
  end,
})

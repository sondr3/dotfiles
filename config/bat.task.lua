local hm = require("heime")

local config = [[ 
--theme="ansi"
]]

return hm.task({
  name = "bat",
  description = "setup and configure bat",
  run = function(ctx)
    ctx:write_string(hm.path(hm.config_dir, "bat", "config"), config)
  end,
})

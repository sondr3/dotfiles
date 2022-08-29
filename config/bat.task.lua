local hm = require("heime")

local config = [[ 
--theme="ansi"
]]

return hm.task({
  name = "bat",
  description = "setup and configure bat",
  run = function(ctx)
    ctx:write_string(ctx:config_file("bat", "config"), config)
  end,
})

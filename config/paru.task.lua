local hm = require("heime")

local template = [[ 
[options]
BottomUp
NewsOnUpgrade
]]

return hm.task({
  name = "paru",
  description = "setup and configure paru",
  enabled = function()
    return hm.is_linux
  end,
  run = function(ctx)
    ctx:write_string(hm.path(hm.config_dir, "paru", "paru.conf"), template)
  end,
})

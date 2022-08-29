local hm = require("heime")

local template = [[ 
[options]
BottomUp
NewsOnUpgrade
]]

return hm.task({
  name = "paru",
  description = "setup and configure paru",
  enabled = function(ctx)
    return ctx.is_linux()
  end,
  run = function(ctx)
    ctx:write_string(ctx:config_file("paru", "paru.conf"), template)
  end,
})

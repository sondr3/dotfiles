local hm = require("heime")

return hm.setup({
  setup = function(ctx)
    ctx:copy_file("config.lua", ctx:config_file("heime", "config.lua"))
  end,
})

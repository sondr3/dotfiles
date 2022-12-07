local hm = require("heime")
local utils = require("heime.utils")

local template = [[ 
{%- if is_macos() -%}
prefix=/Users/sondre/.local
{%- else -%}
prefix=/home/sondre/.local
{%- endif -%}
save-prefix=''
save-exact=true
//registry.npmjs.org/:_authToken={{ npmToken }}
]]

return hm.task({
  name = "node",
  description = "setup and configure node",
  run = function(ctx, data)
    ctx:write_string(ctx:home_file(".npmrc"), utils:template(template, data.data))
  end,
})

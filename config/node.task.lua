local hm = require("heime")
local utils = require("heime.utils")

local template = [[ 
{% if is_macos -%}
prefix=/Users/sondre/.local
{% else -%}
prefix=/home/sondre/.local
{% endif -%}
save-prefix=''
save-exact=true
//registry.npmjs.org/:_authToken={{ npmToken }}

{% if hostname == "venus.local" %}
; begin auth token 
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/registry/:username=zdataProducts 
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/registry/:_password={{ azureRegistryPat }}
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/registry/:email=npm requires email to be set but doesn't use the value
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/:username=zdataProducts 
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/:_password={{ azureRegistryPat }}
//pkgs.dev.azure.com/zdataProducts/_packaging/AritmaFeed/npm/:email=npm requires email to be set but doesn't use the value
; end auth token
{% endif %}
]]

return hm.task({
  name = "node",
  description = "setup and configure node",
  run = function(ctx, data)
    ctx:write(hm.path(hm.home_dir, ".npmrc"), utils:template(template, data()))
  end,
})

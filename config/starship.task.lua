local hm = require("heime")

local config = [[ 
"$schema" = 'https://starship.rs/config-schema.json'

[aws]
disabled = true

[elixir]
disabled = true
 
[purescript]
detect_files = ["spago.yaml", "spago.dhall"]

[haskell]
format = "via [$symbol($ghc_version )]($style)"
]]

return hm.task({
  name = "starship",
  description = "setup and configure starship",
  run = function(ctx)
    ctx:write_string(ctx:config_file("starship.toml"), config)
  end,
})

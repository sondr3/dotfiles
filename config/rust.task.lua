local hm = require("heime")

local config = {
  alias = {
    t = "test --quiet -- --nocapture --color=always",
    br = "build --release",
    rr = "run --release",
    pedantic = "clippy -- -W clippy::pedantic",
    ped = "pedantic",
    nursery = "clippy -- -W clippy::nursery",
    nur = "nursery",
  },
  build = hm.is_linux and {
    rustflags = { "-C", "link-arg=-fuse-ld=lld" },
  } or nil,
  net = {
    ["git-fetch-with-cli"] = true,
  },
}

return hm.task({
  name = "rust",
  description = "setup and configure rust",
  run = function(ctx)
    ctx:write(hm.path(hm.config_dir, "cargo", "config.toml"), hm.to_toml(config))
  end,
})

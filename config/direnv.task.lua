local hm = require("heime")

local config = [[ 
[global]
load_dotenv = true
]]

local direnvrc = [[ 
layout_poetry() {
  if \[\[ ! -f pyproject.toml \]\]; then
    log_error 'No pyproject.toml found. Use `poetry new` or `poetry init` to create one first.'
    exit 2
  fi

  # create venv if it doesn't exist
  poetry run true

  export VIRTUAL_ENV=$(poetry env info --path)
  export POETRY_ACTIVE=1
  PATH_add "$VIRTUAL_ENV/bin"
}
]]

return hm.task({
  name = "direnv",
  description = "setup and configure direnv",
  run = function(ctx)
    ctx:write_string(ctx:config_file("direnv", "direnv.toml"), config)
    ctx:write_string(ctx:config_file("direnv", "direnvrc"), direnvrc)
  end,
})

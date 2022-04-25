local config = require("heime.config")

return {
  data = {
    name = config:get_or_set("name"),
    email = config:get_or_set("email"),
    npmToken = config:get_or_set("npmToken"),
  },
}

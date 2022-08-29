local config = require("heime.config")

return {
  data = {
    name = config:get_or_set("name"),
    email = config:get_or_set("email"),
    sonat_email = config:get_or_set("sonat_email"),
    eviny_email = config:get_or_set("eviny_email"),
    npmToken = config:get_or_set("npmToken"),
  },
}

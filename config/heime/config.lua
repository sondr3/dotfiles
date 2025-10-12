local config = require("heime.config")

return {
	data = {
		name = config:get_or_set("name"),
		email = config:get_or_set("email"),
		work_email = config:get_or_set("work_email"),
		signingkey = config:get_or_set("signingkey"),
		npmToken = config:get_or_set("npmToken", "secret"),
	},
}

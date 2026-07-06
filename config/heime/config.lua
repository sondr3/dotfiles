local config = require("heime.config")

return {
	data = {
		name = config:get_or_set("name"),
		email = config:get_or_set("email"),
		work_email = config:get_or_set("work_email"),
		signingkey = config:get_or_set("signingkey"),
		work_signing_key = config:get_or_set("work_signing_key"),
		npmToken = config:get_or_set("npmToken", "secret"),
		azureRegistryPat = config:get_or_set("azureRegistryPat", "secret"),
		azure_foundry_key = config:get_or_set("azure_foundry_key", "secret"),
		pi_anthropic_url = config:get_or_set("pi_anthropic_url"),
		pi_azure_openai_url = config:get_or_set("pi_azure_openai_url"),
	},
}

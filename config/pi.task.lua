local hm = require("heime")

local auth = function(data)
	return {
		anthropic = {
			type = "api_key",
			key = data:get("azure_foundry_key"),
		},
		["azure-openai-responses"] = {
			type = "api_key",
			key = data:get("azure_foundry_key"),
		},
	}
end

local models = function(data)
	return {
		providers = {
			anthropic = {
				baseUrl = data:get("pi_anthropic_url"),
			},
			["azure-openai-responses"] = {
				baseUrl = data:get("pi_azure_openai_url"),
			},
		},
	}
end

return hm.task({
	name = "pi",
	description = "setup and configure pi.dev",
	run = function(ctx, data)
		ctx:write(hm.path(hm.home_dir, ".pi", "agent", "auth.json"), hm.to_json(auth(data)))
		ctx:write(hm.path(hm.home_dir, ".pi", "agent", "models.json"), hm.to_json(models(data)))
	end,
})

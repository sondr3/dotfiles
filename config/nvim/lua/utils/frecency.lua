local ext = require("telescope._extensions")
local builtins = require("telescope.builtin")
local frecency = require("frecency.db")

local fzf = ext.manager.fzf

local function frecency_score(self, prompt, line, entry)
  if prompt == nil or prompt == "" then
    for _, file_entry in ipairs(self.state.frecency) do
      local filepath = entry.cwd .. "/" .. entry.value
      if file_entry.filename == filepath then
        return 9999 - file_entry.score
      end
    end

    return 9999
  end

  return self.default_scoring_function(self, prompt, line, entry)
end

local function frecency_start(self, prompt)
  self.default_start(self, prompt)

  if not self.state.frecency then
    self.state.frecency = frecency.get_files({})
  end
end

local frecency_sorter = function(opts)
  local fzf_sorter = fzf.native_fzf_sorter()

  fzf_sorter.scoring_function = frecency_score
  fzf_sorter.start = frecency_start

  return fzf_sorter
end

return {
  frecency_sorter = frecency_sorter,
}

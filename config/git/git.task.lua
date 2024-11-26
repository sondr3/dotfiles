local hm = require("heime")

local ssh = function(data)
  local program = nil
  if hm.hostname == "Neptune" then
    program = "/mnt/c/Users/SondreAasemoen/AppData/Local/1Password/app/8/op-ssh-sign-wsl"
  elseif hm.is_linux then
    program = "/opt/1Password/op-ssh-sign"
  else
    program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign"
  end

  return program
end

local config = function(data)
  return {
    user = {
      name = "Sondre Aasemoen",
      email = data:get("email"),
      signingkey = data:get("signingkey"),
    },
    push = {
      default = "current",
      followTags = true,
    },
    pull = {
      default = "current",
      rebase = true,
    },
    rebase = {
      autosquash = true,
    },
    commit = {
      gpgsign = true,
    },
    gpg = {
      format = "ssh",
    },
    [ [[gpg "ssh"]] ] = {
      program = ssh(data),
    },
    advice = {
      addEmptyPathspec = false,
    },
    diff = {
      colorMoved = "default",
    },
    alias = {
      b = "branch -v",
      d = "diff",
      dc = "diff --cached",
      cm = "commit -m",
      can = "commit --amend --no-edit",
      lg = "log --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'",
      r = "remote -v",
      s = "status",
      st = "status -bs",
      co = "!git for-each-ref --format='%(refname:short)' refs/heads | fzf | xargs git checkout",
      last = "log -1 HEAD --stat",
      pp = "!git pull && git push",
      pt = "!git pull && git pull --tags",
      up = "!git push && git push --tags",
      dead = "!git fetch --prune && git branch --merged | rg -v 'main|master' | xargs git branch -d",
      gone = "!git fetch --all --prune && git branch -vv | awk '/: gone]/{print $1}' | xargs git branch -D;",
    },
    github = {
      user = "sondr3",
    },
    core = {
      autocrlf = "input",
      excludesFile = "~/.gitignore",
    },
    init = {
      defaultBranch = "main",
    },
    pager = {
      diff = "delta",
      log = "delta",
      reflog = "delta",
      show = "delta",
    },
    interactive = {
      diffFilter = "delta --color-only",
    },
    delta = {
      features = "side-by-side line-numbers decorations",
      ["whitespace-error-style"] = "22 reverse",
    },
    credentials = {
      helper = "cache",
    },
    rerere = {
      enabled = true,
      autoupdate = true,
    },
    [ [[filter "lfs"]] ] = {
      clean = "git-lfs clean -- %f",
      smudge = "git-lfs smudge -- %f",
      process = "git-lfs filter-process",
      required = true,
    },
  }
end

return hm.task({
  name = "git",
  description = "setup and configure node",
  run = function(ctx, data)
    -- ctx:install({
    --   ["rust-parallel"] = {
    --     windows = "cargo",
    --     macos = "homebrew"
    --   };
    --   "git";
    --   "fzf";
    -- })
    ctx:write(hm.path(hm.home_dir, ".gitconfig"), hm.to_ini(config(data)))

    ctx:copy("gitignore-global", hm.path(hm.home_dir, ".gitignore"))
  end,
})

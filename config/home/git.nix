{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gitAndTools.git-ignore
    gitAndTools.hub
    gitAndTools.lefthook
  ];

  programs.git = {
    enable = true;
    userName = "Sondre Nilsen";
    userEmail = "nilsen.sondre@gmail.com";
    signing = {
      signByDefault = true;
      key = "9CBF84633C7DDB10";
    };

    aliases = {
      b = "branch -v";
      d = "diff";
      dc = "diff --cached";
      cam = "commit --amend --no-edit";
      lg =
        "log --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
      r = "remote -v";
      s = "status";
    };

    extraConfig = {
      push = {
        default = "current";
        followTags = "true";
      };
      pull = {
        default = "current";
        rebase = "true";
      };
      rebase = { autosquash = "true"; };
    };
  };
}

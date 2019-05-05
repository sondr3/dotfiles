{ pkgs, ... }:

{
  home.packages = with pkgs; [
    git-ignore
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
      s = "status";
      b = "branch -v";
      lg = "log --graph --abbrev-commit --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
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
      rebase = {
        autosquash = "true";
      };
    };
  };
}

# Command line tools, terminals etc
{ pkgs, ... }:

with pkgs;

{
  programs = {
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        style = "numbers,changes";
      };
    };
    fzf = {
      enable = true;
      changeDirWidgetCommand = "fd --type d";
      defaultCommand = "fd --type f";
    };
    fish.shellAliases = {
      fzp = ''fzf --preview "bat --color=always {}"'';
      l = "exa -lagFT --git-ignore --git --level 1";
      ls = "exa -a --git-ignore";
      tree = "exa --tree --git-ignore";
    };
  };
  home.packages = [
    (aspellWithDicts (ps: with ps; [ en nb ]))
    alacritty
    broot
    exa
    fd
    httpie
    jq
    jump
    lazygit
    lm_sensors
    neofetch
    nix-prefetch-git
    nix-prefetch-github
    nixpkgs-fmt
    niv
    pandoc
    ripgrep
    tokei
    xclip
    xorg.xkill
    xsv
  ];
}

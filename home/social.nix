{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    slack
    tdesktop
    weechat
  ];
}

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    minecraft
    steam
    teamspeak_client
  ];
}

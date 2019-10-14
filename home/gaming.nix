{ pkgs, ... }:

{
  home.packages = with pkgs; [
    minecraft
    steam
    teamspeak_client
  ];
}

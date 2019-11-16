{ pkgs, ... }:

{
  home.packages = with pkgs; [
    firefox
    google-chrome

    kcharselect
    pavucontrol
    spectacle

    spotify

    skype

    # Super easily create gifs
    peek
  ];
}

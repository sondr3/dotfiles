{ pkgs, config, ... }:

{
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.awesome = {
      enable = true;
      noArgb = true;
    };
  };

  programs.rofi.enable = true;

  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
  };

  home.packages = with pkgs; [
    autorandr

    firefox
    google-chrome

    kcharselect
    pavucontrol
    spectacle

    spotify

    # Super easily create gifs
    peek
  ];
}

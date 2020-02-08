{ pkgs, ... }:

{
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      # extraPackages = p: with p; [ taffybar ];
    };
  };

  programs.rofi.enable = true;

  home.sessionVariables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
  };

  services = {
    # taffybar.enable = true;
    # status-notifier-watcher.enable = true;
  };

  home.packages = with pkgs; [
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

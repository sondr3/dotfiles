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

{ pkgs, config, ... }:

let stable = import <stable> { };
in {
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hpks: [ hpks.taffybar config.mine.taffybar.package ];
      haskellPackages = stable.pkgs.haskell.packages.ghc865;
    };
  };

  mine.taffybar.enable = true;

  programs.rofi.enable = true;

  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
    status-notifier-watcher.enable = true;
  };

  home.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
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

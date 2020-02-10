{ pkgs, ... }:

{
  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;
    # Enable CUPS to print documents.
    printing.enable = true;
    # Enable FSTrim for SSH health
    fstrim.enable = true;
    # Enable power support for applications
    upower.enable = true;

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,no";
      xkbOptions = "grp:alt_caps_toggle";
      desktopManager.xterm.enable = false;
      displayManager = {
        lightdm = { enable = true; };
        session = [{
          name = "xmonad";
          manage = "window";
          start = ''
            ${pkgs.runtimeShell} $HOME/.hm-xsession &
            waitPID=$!
          '';
        }];
      };
    };
  };
}

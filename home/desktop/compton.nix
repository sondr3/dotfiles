{ lib, pkgs, config, ... }:

{
  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "false";
    extraOptions = ''
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
      use-damage = true;
      unredir-if-possible = false;
    '';
  };
}

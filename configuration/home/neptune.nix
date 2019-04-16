{ ... }:

{
  # My desktop machine runs with a DAC/AMP setup but for some reason whenever I
  # lock it the default sink changes to HDMI instead, which is infuriating. This
  # unloads that module, but this is only reasonable on my desktop.
  xdg.configFile."pulse/default.pa".text = ''
    unload-module module-switch-on-port-available
  '';
}

{ pkgs, config, lib, ... }:

{
  config = lib.mkIf config.options.mine.xorg.enable {
    home.packages = with pkgs; [
      discord
      slack
      tdesktop
      weechat
    ];
  };
}

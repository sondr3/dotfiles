{ config, pkgs, lib, ... }:

with lib;
with import ../../lib;

let cfg = config.mine.xmobar;
in {
  options.mine.xmobar = {
    enable = mkEnableOption "xmobar";
    config = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # /nix/store/v82gh9iql5jdknr08ljp428r8bbdqmlr-xmobar-with-packages-8.6.5/bin/xmobar
      # /nix/store/h4g8v8pcd1ld6b7k3ccgf2ddpglqnfpx-ghc-8.6.5-with-packages/bin/xmobar
      # home.packages = [ pkgs.mine.xmobar-wrapped ];
      home.packages = [ pkgs.haskellPackages.xmobar ];
      systemd.user.services.xmobar = {
        Unit = {
          Description = "xmobar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.haskellPackages.xmobar}/bin/xmobar";
          Restart = "on-failure";
        };

        Install = { WantedBy = [ "graphical-session.target" ]; };
      };
    }

    (mkIf (cfg.config != null) {
      xdg.configFile."xmobar/xmobar.hs".source = cfg.config;
      xdg.configFile."xmobar/xmobar.hs".onChange = ''
        echo "Recompiling xmobar"

        $DRY_RUN_CMD systemctl --user restart xmobar.service
      '';
    })
  ]);
}

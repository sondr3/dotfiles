{ config, pkgs, lib, ... }:

with lib;
with import ../../../lib;

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
      home.packages = [ pkgs.xmobar ];
      systemd.user.services.xmobar = {
        Unit = {
          Description = "xmobar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart =
            "${pkgs.xmobar}/bin/xmobar /etc/nixos/config/home/desktop/xmobar.hs";
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

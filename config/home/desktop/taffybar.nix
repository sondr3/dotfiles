{ config, pkgs, lib, ... }:

with lib;

let cfg = config.mine.taffybar;
in {
  options.mine.taffybar = {
    enable = mkEnableOption "taffybar";
    package = mkOption {
      description = "Taffybar package to use";
      default = (import ./taffybar { }).mytaffybar;
      example = pkgs.taffybar;
    };
    config = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ (lowPrio cfg.package) ];
      systemd.user.services.taffybar = {
        Unit = {
          Description = "Taffybar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${cfg.package}/bin/mytaffybar";
          Restart = "on-failure";
        };

        Install = { WantedBy = [ "graphical-session.target" ]; };
      };
    }

    (mkIf (cfg.config != null) {
      xdg.configFile."taffybar/taffybar.hs".source = cfg.config;
      xdg.configFile."taffybar/taffybar.hs".onChange = ''
        echo "Recompiling Taffybar"

        $DRY_RUN_CMD rm -rf $HOME/.cache/taffybar/
        $DRY_RUN_CMD systemctl --user restart taffybar.service
      '';
    })
  ]);
}

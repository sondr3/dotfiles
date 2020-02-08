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
  };

  config = mkIf cfg.enable {

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
  };
}

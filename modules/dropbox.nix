{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.dropbox;
in {
  options.services.dropbox = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = true;
      description = ''
        Enable DropBox CLI for Linux.
      '';
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.sondre = {
      home.packages = with pkgs;
        [
          # dropbox - we don't need this in the environment. systemd unit pulls it in
          dropbox-cli
        ];
    };

    networking.firewall = {
      allowedTCPPorts = [ 17500 ];
      allowedUDPPorts = [ 17500 ];
    };

    systemd.user.services.dropbox = {
      description = "Dropbox";
      wantedBy = [ "graphical-session.target" ];
      environment = {
        QT_PLUGIN_PATH = "/run/current-system/sw/"
          + pkgs.qt5.qtbase.qtPluginPrefix;
        QML2_IMPORT_PATH = "/run/current-system/sw/"
          + pkgs.qt5.qtbase.qtQmlPrefix;
      };
      serviceConfig = {
        ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
        ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
        KillMode = "control-group"; # upstream recommends process
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;
      };
    };
  };
}
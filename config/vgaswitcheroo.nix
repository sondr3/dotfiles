{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.vgaswitcheroo;
in
{
  options.hardware.vgaswitcheroo = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = true;
      description = ''
        Completely disable the dedicated graphics card and use the
        integrated graphics processor instead.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.vgaswitcheroo = {
      description = "Deactivate the discrete GPU on boot";
      path = [ pkgs.bash ];
      after = [ "sys-kernel-debut.mount" ];
      before = [ "systemd-vconsole-setup.service" "display-manager.service" ];
      requires = [ "sys-kernel-debug.mount" "vgaswitcheroo.path" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart = "${pkgs.bash}/bin/sh -c 'echo -e \"DIGD\\nOFF\" > /sys/kernel/debug/vgaswitcheroo/switch'";
        ExecStop = "${pkgs.bash}/bin/sh -c 'echo ON > /sys/kernel/debug/vgaswitcheroo/switch'";
      };
    };
    systemd.paths."vgaswitcheroo" = {
      pathConfig = {
        PathExists = "/sys/kernel/debug/vgaswitcheroo/switch";
        Unit = "vgaswitcheroo.service";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}

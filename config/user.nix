{ lib, pkgs, ... }:

with lib;

let
  cfg = options.mine.sondre;
in
{
  options.mine.sondre.enable = mkEnableOption "Enable me";

  config = mkIf cfg.enable {
    users.users.sondre = {
      isNormalUser = true;
      description = "Sondre Nilsen";
      extraGroups = [ "wheel" "networkmanager" "docker" "fuse" ];
      shell = pkgs.fish;
    };
  };
}

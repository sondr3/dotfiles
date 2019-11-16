{ lib, pkgs, ... }:

with lib;

{
  options.mine.sondre.enable = mkEnableOption "Enable me";

  config = mkIf options.mine.sondre.enable {
    users.users.sondre = {
      isNormalUser = true;
      description = "Sondre Nilsen";
      extraGroups = [ "wheel" "networkmanager" "docker" "fuse" ];
      shell = pkgs.fish;
    };
  };
}

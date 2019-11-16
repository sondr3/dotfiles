{ lib, pkgs, ... }:

{
  options.mine.sondre.enable = lib.mkEnableOption "Enable me";

  users.users.sondre = {
    isNormalUser = true;
    description = "Sondre Nilsen";
    extraGroups = [ "wheel" "networkmanager" "docker" "fuse" ];
    shell = pkgs.fish;
  };
}

{ pkgs, ... }:

let sources = import ../../nix/sources.nix;
in {
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sondre = {
    isNormalUser = true;
    description = "Sondre Nilsen";
    extraGroups = [ "wheel" "networkmanager" "docker" "fuse" ];
    shell = pkgs.fish;
  };

  home-manager.users.sondre = { pkgs, ... }: {
    nixpkgs = {
      overlays = [ (import ../pkgs) ];
      config.packageOverrides = pkgs: {
        mine = import sources.nix-expressions { inherit pkgs; };
      };
    };
    imports = [
      ../home
      # [TODO] Fix this bloody import
      ../home/desktop/xmobar.nix
    ];
  };
}

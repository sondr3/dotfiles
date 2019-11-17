{ pkgs, ... }:

let
  unstable = import <unstable> {};
in

{
  # TODO: Clean this up, use `home-manager`
  programs = {
    fish.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    mtr.enable = true;
    vim.defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    acpi
    bind
    binutils
    cachix
    coreutils
    curl
    gitAndTools.gitFull
    htop
    unstable.niv
    pciutils
    psmisc
    tree
    unzip
    wget
    zip
  ];
}

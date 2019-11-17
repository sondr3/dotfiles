{ pkgs, config, lib, ... }:

with lib;

let
  cfg = options.mine.userPackages;
  unstable = import <unstable> {};
in
{
  options.mine.userPackages.enable = mkEnableOption "User packages";

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
    httpie
    jq
    lm_sensors
    pciutils
    psmisc
    ripgrep
    tokei
    tree
    unzip
    wget
    xsv
    zip
  ];

  config = mkIf cfg.enable {
    home.packages = [
      (aspellWithDicts (ps: with ps; [ en nb ]))
      alacritty
      jump
      neofetch
      nix-prefetch-git
      nix-prefetch-github
      nixpkgs-fmt
      pandoc
      unstable.niv
      xclip
      xorg.xkill
    ];
  };
}

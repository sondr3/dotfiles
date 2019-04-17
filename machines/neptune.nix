{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./neptune-hardware.nix
      # Add home-manager module
      "${builtins.fetchGit {
        ref = "release-19.03";
        url = "https://github.com/rycee/home-manager";
      }}/nixos"
    ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = pkgs.linuxPackages_5_0;
  boot.kernelModules = [ "amdgpu.dc=1" ];
  boot.cleanTmpDir = true;
  boot.plymouth.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.cpu.amd.updateMicrocode = true;

  networking.hostName = "neptune"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fontconfig.dpi = 221;
    fonts = with pkgs; [
      corefonts
      inconsolata
      fira
      fira-mono
      dejavu_fonts
      libertine
      ubuntu_font_family
      noto-fonts
      source-code-pro
    ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # core tools
    coreutils gitAndTools.gitFull htop
    wget curl zip unzip tree ranger
    acpi psmisc pciutils bind

    # editing
    neovim

    # useful
    gnupg unzip

    # development
    binutils gcc gnumake openssl pkgconfig
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    8000 # Assorted web stuff
    3000 # React
  ];
  networking.firewall.allowedUDPPorts = [
    34197 # Factorio
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.extraConfig = ''
    # Required because it keeps switching to HDMI all the fucking time
    unload-module module-switch-on-port-available
  '';

  # OpenGL
  hardware.opengl.driSupport32Bit = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.layout = "us,no";
  services.xserver.xkbOptions = "grp:alt_caps_toggle";

  services.redshift.enable = true;
  services.redshift.latitude = "63.39";
  services.redshift.longitude = "5.33";
  services.redshift.temperature.day = 6500;
  services.redshift.temperature.night = 2300;
  services.fstrim.enable = true;


  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sondre = {
    isNormalUser = true;
    description = "Sondre Nilsen";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };

  home-manager.users.sondre = { pkgs, ... }: {
    imports = [
      # Import all home configurations
      ../configuration/module-list.nix
      # We can game on this machine
      ../configuration/home/gaming.nix
    ];

    home.packages = with pkgs; [
      httpie
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}

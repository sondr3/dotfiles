{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./uranus-hardware.nix
      # Add home-manager module
      "${builtins.fetchGit {
        ref = "release-19.03";
        url = "https://github.com/rycee/home-manager";
      }}/nixos"
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelPackages = pkgs.linuxPackages_5_2;
    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    kernelParams = [
      "mem_sleep_default=deep"
      "i915.enable_dc=2"
      "i915.enable_fbc=1"
      "i915.enable_psr=1"
      "i915.enable_guc=2"
    ];
    cleanTmpDir = true;
    plymouth.enable = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;

    # Enable sound.
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };

   # Bluetooth
   bluetooth = {
     enable = true;
     package = pkgs.bluezFull;
   };

    # OpenGL
    opengl = {
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel vaapiVdpau libvdpau-va-gl
      ];
    };
  };

  networking = {
    hostName = "uranus";
    networkmanager.enable = true;

    firewall.allowedTCPPorts = [
      8000 # Assorted web stuff
      3000 # React
    ];
  };

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
  programs = {
    fish.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    mtr.enable = true;
  };

  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;
    # Enable CUPS to print documents.
    printing.enable = true;
    # Enable FSTrim for SSH health
    fstrim.enable = true;
    # Enable TLP for power saving
    tlp.enable = true;
    # Enable firmware updating
    fwupd.enable = true;

    redshift.enable = true;
    redshift.latitude = "63.39";
    redshift.longitude = "5.33";
    redshift.temperature.day = 6500;
    redshift.temperature.night = 2300;

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,no";
      xkbOptions = "grp:alt_caps_toggle";

      # Enable the KDE Desktop Environment.
      libinput.enable = true;
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
    };
  };

  powerManagement = {
    cpuFreqGovernor = null;
    # Enable PowerTop auto-tuning
    powertop.enable = true;
  };

  # Enable Docker
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.etc."fuse.conf".text = ''
    # Allow non-root users to specify the 'allow_other' or 'allow_root'
    # mount options.
    user_allow_other
  '';

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sondre = {
    isNormalUser = true;
    description = "Sondre Nilsen";
    extraGroups = [ "wheel" "networkmanager" "docker" "fuse" ];
    shell = pkgs.fish;
  };

  home-manager.users.sondre = { pkgs, ... }: {

    nixpkgs.overlays = [ (import ../pkgs/default.nix) ];

    imports = [
      # Import all home configurations
      ../configuration/module-list.nix
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

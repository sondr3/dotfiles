# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelModules = ["applesmc"];
    cleanTmpDir = true;
    extraModprobeConfig = ''
      options snd-hda-intel model=mbp101
    '';

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    plymouth.enable = true;
  };

  # boot.kernelModules = ["applesmc"];
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.cleanTmpDir = true;

  # networking.hostName = "jupiter"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking = {
    hostName = "jupiter";
    # connman.enable = true;
    enableB43Firmware = true;
    # wireless.enable = true;
    # wicd.enable = true;
    networkmanager.enable = true;
  };

  sound.enable = true;

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
  };

  # hardware.bluetooth.enable = true; # enable BlueTooth for wireless stuff
  # hardware.pulseaudio.enable = true; # enable PulseAudio for audio
  # hardware.enableAllFirmware = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

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

  # enable virtualbox
  # virtualisation.virtualbox.guest.enable = true;
  # virtualisation.virtualbox.host.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # core tools
    coreutils gitAndTools.gitFull htop
    wget curl zip unzip tree ranger
    acpi psmisc pciutils

    # bspwm related
    networkmanagerapplet wmname libnotify
    dunst rofi polybar i3lock-color

    # latex
    texlive.combined.scheme-full

    # rust alternatives to core tools
    ripgrep exa fd

    # visual
    i3lock-color

    # languages
    python3Full lua mariadb

    # editors
    neovim emacs 
    aspell aspellDicts.en aspellDicts.nb

    # terminals
    termite neofetch

    # etc
    firefox playerctl zathura sxiv pavucontrol lm_sensors

    # fix themes
    breeze-gtk gnome3.gnome_themes_standard elementary-icon-theme
    lxappearance
  ];

  environment.variables = {
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "0.5";
    _JAVA_AWT_WM_NONREPARTENTING = "1";
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dsun.java2d.opengl=true";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.

  services = {
    xserver = {
      enable = true;
      videoDrivers = ["nvidia"];
      dpi = 221;
      exportConfiguration = true;

      deviceSection = ''
        Option  "Coolbits" "12"
        Option "NoLogo"
        Option "RegistryDwords" "EnableBrightnessControl=1; PowerMizerEnable=0x1"
      '';
      # Option "RegistryDwords" "PowerMizerEnable=0x1; PerfLevelSrc=0x2222; PowerMizerLevel=0x3; PowerMizerDefault=0x3; PowerMizerDefaultAC=0x3; EnableBrightnessControl=1"

      layout = "us,no";
      xkbVariant = "mac";
      xkbModel = "macbook79";
      xkbOptions = "grp:alt_caps_toggle";
      enableCtrlAltBackspace = true;

      # windowManager.awesome.enable = true;
      windowManager.bspwm.enable = true;
      displayManager.lightdm.enable = true;

      xautolock = {
        enable = true;
        enableNotifier = true;
        locker = "${pkgs.i3lock-color}/bin/i3lock-color --blur=sigma";
        notifier = "${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'";
        time = 10;
      };

      libinput = {
        enable = true;
	      naturalScrolling = true;
      };
    };

    printing = {
      enable = true;
    };

    compton = {
      enable = true;
      vSync = "opengl-swc";
      extraOptions = ''
        paint-on-overlay = true;
        glx-no-stencil = true;
        glx-copy-from-front = false;
        glx-no-rebind-pixmap = true;
        glx-swap-method = "exchange";
        dbe = false;
        sw-opti = true;
        xrender-sync-fence = true;
      '';
    };

    redshift = {
      enable = true;
      provider = "geoclue2";

      temperature = {
        day = 5500;
        night = 3500;
      };
    };

    thermald.enable = true;
    acpid.enable = true;
    xbanish.enable = true;

    mbpfan = {
      enable = true;
      minFanSpeed = 2000;
      maxFanSpeed = 5940;
      lowTemp = 50;
      highTemp = 60;
      maxTemp = 87;
    };
  };

  # services.xserver.enable = true;
  # services.xserver.videoDrivers = ["nvidia"];
  # services.xserver.dpi = 221;
  # services.xserver.layout = "us";
  # services.xserver.xkbVariant = "mac";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
  };
  
  powerManagement = {
    enable = true;
    # Already enable in hardware-configuration.nix
    # cpuFreqGovernor = "powersave";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs = {
    light.enable = true;
    kbdlight.enable = true;
    mtr.enable = true;
    fish.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  virtualisation = {
    docker.enable = true;
    virtualbox = {
      host.enable = true;
      guest.enable = true;
    };
  };

  # programs.light.enable = true;
  # programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.sondre = {
    isNormalUser = true;
    uid = 1001;
    group = "users";
    description = "Sondre Nilsen";
    extraGroups = [
      "wheel"
      "networkmanager"
      "vboxusers"
      "docker"
    ];
    createHome = true;
    home = "/home/sondre";
    shell = "/run/current-system/sw/bin/fish";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}

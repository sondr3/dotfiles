{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # Disable the fucking Nvidia card
    ./vgaswitcheroo.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelModules = ["applesmc"];
    kernelParams = ["i915.modeset=1"];
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

  networking = {
    hostName = "jupiter";
    enableB43Firmware = true;
    networkmanager.enable = true;
  };

  sound.enable = true;

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
    vgaswitcheroo.enable = true;
    opengl = {
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiIntel libvdpau-va-gl vaapiVdpau
      ];
      extraPackages32 = with pkgs.pkgsi686Linux; [
        vaapiIntel libvdpau-va-gl vaapiVdpau
      ];
    };
  };

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

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # core tools
    coreutils gitAndTools.gitFull htop
    wget curl zip unzip tree ranger
    acpi psmisc pciutils

    # bspwm related
    networkmanagerapplet wmname libnotify
    dunst rofi polybar i3lock-pixeled

    # latex
    texlive.combined.scheme-full

    # rust alternatives to core tools
    ripgrep exa fd

    # languages
    python3Full lua mariadb

    # editors
    neovim emacs 
    aspell aspellDicts.en aspellDicts.nb

    # terminals
    termite neofetch

    # useful
    gnupg

    # etc
    firefox playerctl zathura sxiv pavucontrol lm_sensors

    # fix themes
    breeze-gtk gnome3.gnome_themes_standard elementary-icon-theme
    lxappearance
  ];

  environment.variables = {
    LIBVA_DRIVER_NAME = "i965";
    VDPAU_DRIVER = "va_gl";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "0.5";
    _JAVA_AWT_WM_NONREPARTENTING = "1";
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dsun.java2d.opengl=true";
  };

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "modesetting" ];
      dpi = 221;
      exportConfiguration = true;

      layout = "us,no";
      xkbVariant = "mac";
      xkbModel = "macbook79";
      xkbOptions = "grp:alt_caps_toggle";
      enableCtrlAltBackspace = true;

      windowManager.bspwm.enable = true;
      displayManager.lightdm.enable = true;

      xautolock = {
        enable = true;
        locker = "${pkgs.i3lock-pixeled}/bin/i3lock-pixeled";
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
      backend = "glx";
      vSync = "opengl";
      extraOptions = ''
        paint-on-overlay = true;
        glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
        glx-swap-method = "buffer-age";
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

  nixpkgs.config = {
    allowUnfree = true;
  };
  
  powerManagement = {
    enable = true;
    # Already enable in hardware-configuration.nix
    # cpuFreqGovernor = "powersave";
  };

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
  };

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

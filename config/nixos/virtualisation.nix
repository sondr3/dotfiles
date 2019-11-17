{
  # Enable Docker
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };

    virtualbox = {
      host.enable = false;
      host.enableExtensionPack = false;
      guest.enable = false;
    };
  };
}

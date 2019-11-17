{
  nix = {
    autoOptimiseStore = true;
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };
}

{ ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
  };
}

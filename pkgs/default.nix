self: super:

{
  jump = super.callPackage ./jump.nix {};
  cargo-outdated = super.callPackage ./cargo-outdated.nix {};
}

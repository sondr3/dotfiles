self: super:

{
  jump = super.callPackage ./jump.nix {};
  cargo-outdated = super.callPackage ./cargo-outdated.nix {};
  git-ignore = super.callPackage ./git-ignore.nix {};
}

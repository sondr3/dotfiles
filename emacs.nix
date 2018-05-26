{ pkgs, ...}:

let
  emacs26 = pkgs.stdenv.lib.overrideDerivation pkgs.emacs (oldAttrs : {
    version = "26.1RC1";
    src = pkgs.fetchurl {
      url = ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz;
      sha256 = "6594e668de00b96e73ad4f168c897fe4bca7c55a4caf19ee20eac54b62a05758";
    };
    withGTK3 = true;
    withGTK2 = false;
    patches = [];
  });
  emacs = emacs26;
in
{
  environment.systemPackages = [
      emacs
  ];

  services.emacs.package = emacs;
  services.emacs.enable = true;
}

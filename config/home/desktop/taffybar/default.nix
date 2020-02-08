{ pkgs ? import <stable> { } }:

with pkgs.haskell.lib;

let hpkgs = pkgs.haskell.packages.ghc865;

in (hpkgs.extend (packageSourceOverrides { mytaffybar = ./.; })).extend
(self: super: {
  mytaffybar = super.mytaffybar.overrideAttrs (drv: {
    nativeBuildInputs = drv.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
    postInstall = drv.postInstall or "" + ''
      wrapProgram $out/bin/mytaffybar \
        --set GDK_PIXBUF_MODULE_FILE "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
    '';
  });
}) // {
  inherit pkgs;
}

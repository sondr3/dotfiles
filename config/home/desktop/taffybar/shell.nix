with import ./. { };

(shellFor {
  packages = pkgs: with pkgs; [ mytaffybar ];
  withHoole = true;
  buildInputs = with pkgs; [ hicolor-icon-theme ];
  nativeBuildInputs = with pkgs; [ cabal-install ];
}).overrideAttrs (drv: {
  shellHook = ''
    export GDK_PIXBUF_MODULE_FILE="${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
  '';
})

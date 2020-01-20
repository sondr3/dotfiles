self: super:

{
  neovim-unwrapped = (super.neovim-unwrapped.override { lua = self.luajit; }).overrideAttrs (
    oldAttrs: {
      version = "nightly";
      src = builtins.fetchGit {
        url = https://github.com/neovim/neovim.git;
        ref = "master";
      };

      buildInputs = oldAttrs.buildInputs ++ [
        self.pkgs.icu
        self.pkgs.utf8proc
      ];
    }
  );
}

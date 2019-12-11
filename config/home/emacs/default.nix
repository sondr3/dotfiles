{ config, pkgs, lib, ... }:

with import ../../../lib;

let
  amalthea = (
    pkgs.emacs.override {
      withGTK3 = true;
      withGTK2 = false;
    }
  );
in
{
  nixpkgs.overlays = [
    (
      import (
        builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }
      )
    )
  ];
  programs.emacs = {
    enable = true;
    package = amalthea;
    extraPackages = (
      epkgs: (
        with epkgs.elpaPackages; [
          aggressive-indent
          auctex
          delight
          undo-tree
        ]
      ) ++ (
        with epkgs.melpaPackages; [
          amx
          apropospriate-theme
          auctex-latexmk
          auto-compile
          company
          company-auctex
          company-lsp
          company-math
          company-nginx
          company-quickhelp
          company-reftex
          company-shell
          company-statistics
          counsel
          counsel-projectile
          deadgrep
          dhall-mode
          diff-hl
          direnv
          dockerfile-mode
          ebib
          evil
          evil-collection
          evil-commentary
          evil-goggles
          evil-lion
          evil-magit
          evil-smartparens
          evil-surround
          exec-path-from-shell
          fish-mode
          flycheck
          flycheck-haskell
          flyspell-correct-ivy
          general
          gitattributes-mode
          gitconfig-mode
          gitignore-mode
          haskell-mode
          helpful
          hl-todo
          hlint-refactor
          hydra
          ivy
          ivy-bibtex
          ivy-yasnippet
          json-mode
          latex-extra
          lsp-haskell
          lsp-ivy
          lsp-mode
          lsp-ui
          macrostep
          magic-latex-buffer
          magit
          markdown-mode
          nasm-mode
          nginx-mode
          nix-mode
          no-littering
          org-ref
          org-super-agenda
          outshine
          prettier-js
          projectile
          rainbow-delimiters
          rust-mode
          smartparens
          swiper
          typescript-mode
          use-package
          which-key
          ws-butler
          yaml-mode
          yasnippet
          yasnippet-snippets
        ]
      ) ++ (
        with epkgs.orgPackages; [
          org
          org-plus-contrib
        ]
      ) ++ (
        with epkgs; [
          emacs-libvterm
        ]
      )
    );
  };

  home.activation.emacs = execute ''
    ln -sfT /etc/nixos/config/home/emacs/ ~/.emacs.d
    emacs --batch -l ~/.emacs.d/init.el --eval "(amalthea--byte-compile-amalthea)"
  '';
}

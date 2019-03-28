{ pkgs ? import <nixpkgs> {} }:

let
  amalthea = (pkgs.emacs.override {
    withGTK3 = true;
    withGTK2 = false;
  });
  emacsWithPackages = (pkgs.emacsPackagesNgGen amalthea).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.elpaPackages; [
    aggressive-indent
    auctex
    delight
    undo-tree
  ]) ++ (with epkgs.melpaPackages; [
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
    diff-hl
    dockerfile-mode
    ebib
    evil
    evil-collection
    evil-commentary
    evil-goggles
    evil-lion
    evil-magit
    evil-surround
    exec-path-from-shell
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
    intero
    ivy
    ivy-bibtex
    ivy-yasnippet
    json-mode
    latex-extra
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
    swiper
    typescript-mode
    use-package
    which-key
    ws-butler
    yaml-mode
    yasnippet
    yasnippet-snippets
  ]) ++ (with epkgs.orgPackages; [
    org
    org-plus-contrib
  ]) ++ [

  ])

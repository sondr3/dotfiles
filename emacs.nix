{ pkgs ? import <nixpkgs> {} }:

let
  amalthea = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen amalthea).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.elpaPackages; [
    delight
    async
    undo-tree
    aggressive-indent
    auctex
    hydra
  ]) ++ (with epkgs.melpaPackages; [
    use-package
    which-key
    general
    exec-path-from-shell
    projectile
    counsel-projectile
    evil
    evil-collection
    evil-lion
    evil-goggles
    evil-surround
    evil-commentary
    evil-smartparens
    evil-magit
    magit
    diff-hl
    hl-todo
    helpful
    rainbow-delimiters
    smartparens
    ws-butler
    ivy
    counsel
    swiper
    amx
    company
    company-quickhelp
    company-statistics
    yasnippet
    yasnippet-snippets
    ivy-yasnippet
    deadgrep
    flyspell-correct-ivy
    flycheck
    apropospriate-theme
    org-ref
    nginx-mode
    company-nginx
    yaml-mode
    dockerfile-mode
    nasm-mode
    auto-compile
    macrostep
    rust-mode
    json-mode
    markdown-mode
    auctex-latexmk
    company-auctex
    company-math
    magic-latex-buffer
    latex-extra
    company-reftex
    ivy-bibtex
    ebib
    company-shell
    haskell-mode
    intero
    flycheck-haskell
    hlint-refactor
    nix-mode
  ]) ++ (with epkgs.orgPackages; [
    org
    org-plus-contrib
  ]) ++ [

  ])

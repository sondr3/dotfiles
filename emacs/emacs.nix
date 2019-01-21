{ pkgs ? import <nixpkgs> {} }:

let
  amalthea = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen amalthea).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.elpaPackages; [
    aggressive-indent
    auctex
    delight
    undo-tree
  ]) ++ (with epkgs.melpaStablePackages; [
    auto-compile
    company
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    hl-todo
    hydra
    macrostep
    magit
    markdown-mode
    no-littering
    org-super-agenda
    projectile
    yasnippet
  ]) ++ (with epkgs.melpaPackages; [
    amx
    apropospriate-theme
    auctex-latexmk
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
    evil-smartparens
    evil-surround
    exec-path-from-shell
    flycheck
    flycheck-haskell
    flyspell-correct-ivy
    general
    haskell-mode
    helpful
    hlint-refactor
    intero
    ivy
    ivy-bibtex
    ivy-yasnippet
    json-mode
    latex-extra
    lsp-mode
    lsp-ui
    magic-latex-buffer
    nasm-mode
    nginx-mode
    nix-mode
    org-ref
    outshine
    prettier-js
    rainbow-delimiters
    rust-mode
    smartparens
    swiper
    typescript-mode
    use-package
    which-key
    ws-butler
    yaml-mode
    yasnippet-snippets
  ]) ++ (with epkgs.orgPackages; [
    org
    org-plus-contrib
  ]) ++ [

  ])

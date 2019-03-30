{ pkgs, ... }:

{
  home.packages = if !pkgs.stdenv.isDarwin
  then (with pkgs; [
    jetbrains.webstorm
    jetbrains.pycharm-professional
    jetbrains.clion
    jetbrains.idea-ultimate
  ]) else [];

  home.file.".ideavimrc".text = ''
    set surround
    set hlsearch
    set incsearch
    set ignorecase
    set smartcase
    set incsearch
    set showmode
    set number
    set relativenumber

    let mapleader = " "

    imap jk <esc>

    nmap <leader>r :action Run<cr>
    nmap <leader>t :action Refactorings.QuickListPopupAction<cr>
    nmap <leader>b :action CompileDirty<cr>
  '';
}

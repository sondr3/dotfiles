# Dotfiles

You know the drill.

## `emacs.d`

*NOTE:* I don't use Emacs anymore, this was just an attempt by me to see
whether I could replace Vim with it. And as it turns out I couldn't, so these
are pretty much just the leftovers of my attempt to use Emacs. Who knows, I
might just go back and retry it again in the future, but right now I'm very
happy with Vim and looking forward to trying [Neovim](neovim).

## `vim`

Installed with [Homebrew](homebrew): `brew install vim --with-lua
--with-luajit --HEAD`. Plugins are managed with [vim-plug](vimplug).

The `vim` folder contains all the plugins and color schemes and such, as
you'd expect while the `vimrc` gets symlinked `~/.vimrc` and `vim`
gets symlinked to `~/.vim`. The settings aren't too spectacular, mostly just
formatting and such, but feel free to take a look at it.

## `tmux`

Same as with `vim`, I install it with [Homebrew](homebrew): `brew install
tmux`, however I also install [tmux-MacOSX-pasteboard](pasteboard) to get
pasting to properly work. To manage plugins I use [tpm](tpm) (Tmux Plugin
Manager). And as with vim the `tmux.conf` gets symlinked to
`~/.tmux.conf`.

[homebrew]: http://brew.sh/
[vimplug]: https://github.com/junegunn/vim-plug
[neovim]: https://github.com/neovim/neovim/
[pasteboard]: https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard
[tpm]: https://github.com/tmux-plugins/tpm

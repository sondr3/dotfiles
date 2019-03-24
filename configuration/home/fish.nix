{ ... }:

{
  programs.fish = {
    enable = true;
    shellInit = ''
      # TODO: Create better greeting
      set fish_greeting
    '';
  };
}

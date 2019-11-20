{
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    fish.enable = true;
    mtr.enable = true;
    vim.defaultEditor = true;
  };
}

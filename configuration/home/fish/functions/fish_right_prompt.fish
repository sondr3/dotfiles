set -g __amalthea_nix_symbol "❆"

function __amalthea_nix_prompt --description "Add Nix icon if inside Nix shell"
  [ -z "$IN_NIX_SHELL" ]; and return
  set_color white
  echo -ns "$__amalthea_nix_symbol "
  set_color normal
end

function fish_right_prompt
  __amalthea_nix_prompt
end

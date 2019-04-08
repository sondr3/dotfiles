set -g __amalthea_nix_symbol "❆"

function fish_right_prompt
  if test -n "$IN_NIX_SHELL"
    set_color white
    echo -ns "$__amalthea_nix_symbol "
  end
end

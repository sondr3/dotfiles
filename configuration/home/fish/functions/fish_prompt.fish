set -g __amalthea_prompt_symbol "❯"
set -g __amalthea_git_symbol ""
set -g __amalthea_nix_symbol ""
set -g __amalthea_username "sondre"

function __amalthea_git_branch --description "Show the current branch"
  echo (command git rev-parse --abbrev-ref HEAD 2>/dev/null)
end

function fish_prompt
  set -l last_status $status
  set -l git_status (__amalthea_git_branch)

  echo -e ""

  if test $USER = "root"
    set_color red
    echo -n $USER@
  else if test $USER != $__amalthea_username
    set_color white
    echo -n $USER@
  end

  echo -ne " " (prompt_pwd) " "

  set_color normal

  if test -n "$git_status"
    set_color yellow
    echo -n $__amalthea_git_symbol $git_status
  end

  echo -ne " "
  set_color normal

  if test -n "$IN_NIX_SHELL"
    set_color cyan
    echo -n $__amalthea_nix_symbol
  end

  set_color normal

  echo " "

  set_color cyan
  echo -n "$__amalthea_prompt_symbol "
  set_color normal
end

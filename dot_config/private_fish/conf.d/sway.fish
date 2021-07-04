set TTY1 (tty)
if test -z "$DISPLAY"; and test $TTY1 = "/dev/tty1"
    set -gx MOZ_ENABLE_WAYLAND 1
    exec sway
end

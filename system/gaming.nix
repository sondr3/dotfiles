{ pkgs, ... }:

{
  # OpenGL support for Steam etc
  hardware = {
    opengl.driSupport32Bit = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    pulseaudio.support32Bit = true;
  };
}

{ pkgs, ... }:

{
  home.packages = with pkgs; [ streamlink youtube-dl ];

  programs.mpv = {
    enable = true;
    config = {
      profile = "gpu-hq";
      gpu-api = "vulkan";
      scale = "ewa_lanczossharp";
      cscale = "ewa_lanczossharp";
      dscale = "mitchell";
      video-sync = "display-resample";
      interpolation = "yes";
      tscale = "box";
      tscale-window = "sphinx";
      tscale-radius = 1.01;
      tscale-clamp = 0.0;
      demuxer-mkv-subtitle-preroll = "yes";
      sub-ass-vsfilter-blur-compat = "no";
      sub-auto = "fuzzy";
    };
  };
}

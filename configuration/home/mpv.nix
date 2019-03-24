{ pkgs, ... }:

{
  home.packages = with pkgs; [
    mpv
    youtube-dl
  ];

  xdg.configFile."mpv/mpv.conf".text = ''
    # gpu high profile with even more quality scalers
    profile=gpu-hq
    gpu-api=vulkan
    scale=ewa_lanczossharp
    cscale=ewa_lanczossharp
    dscale=mitchell

    # best interpolation
    video-sync=display-resample
    interpolation=yes
    tscale=box
    tscale-window=sphinx
    tscale-radius=1.01
    tscale-clamp=0.0

    demuxer-mkv-subtitle-preroll=yes
    sub-ass-vsfilter-blur-compat=no
    sub-auto=fuzzy
  '';
}

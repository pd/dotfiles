{ pkgs, ... }:
{
  # various tools needed for music library management
  home.packages = with pkgs; [
    flac
    imagemagick
    intermodal
    lame
    mktorrent
  ];
}

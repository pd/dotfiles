{ pkgs, ... }:
{
  home.packages = with pkgs; [
    flac
    imagemagick
    lame
    mktorrent
    pyrosimple # lstor, rtxmlrpc
  ];
}

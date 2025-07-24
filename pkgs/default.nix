{ pkgs, ... }:
{
  filebotd = pkgs.callPackage ./filebotd { };
  rtorrent-exporter = pkgs.callPackage ./rtorrent-exporter.nix { };
}

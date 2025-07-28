{ pkgs, ... }:
{
  filebotd = pkgs.callPackage ./filebotd { };
  mediaman = pkgs.callPackage ./mediaman { };
  rtorrent-exporter = pkgs.callPackage ./rtorrent-exporter.nix { };
  waybar-dunst = pkgs.callPackage ./waybar-dunst.nix { };
}

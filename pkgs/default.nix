{ pkgs, ... }:
{
  filebotd = pkgs.callPackage ./filebotd { };
  mediaman = pkgs.callPackage ./mediaman { };
  ptpimg-uploader = pkgs.callPackage ./ptpimg-uploader.nix { };
  rtorrent-exporter = pkgs.callPackage ./rtorrent-exporter.nix { };
  waybar-dunst = pkgs.callPackage ./waybar-dunst.nix { };
}

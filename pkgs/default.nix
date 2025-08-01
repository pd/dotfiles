{ pkgs, ... }:
{
  dns-blocklist = pkgs.callPackage ./dns-blocklist.nix {
    inherit (pkgs.unstable) stevenblack-blocklist;
  };

  filebot = pkgs.callPackage ./filebot.nix { };
  filebotd = pkgs.callPackage ./filebotd { };
  mediaman = pkgs.callPackage ./mediaman { };
  ptpimg-uploader = pkgs.callPackage ./ptpimg-uploader.nix { };
  rtorrent-exporter = pkgs.callPackage ./rtorrent-exporter.nix { };
  waybar-dunst = pkgs.callPackage ./waybar-dunst.nix { };
  xtor = pkgs.callPackage ./xtor.nix { };
}

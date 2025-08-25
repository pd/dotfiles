{ pkgs, unstable, ... }:
{
  dns-blocklist = pkgs.callPackage ./dns-blocklist.nix {
    inherit (unstable) stevenblack-blocklist;
  };

  filebotd = pkgs.callPackage ./filebotd { };
  mediaman = pkgs.callPackage ./mediaman { };
  npd = pkgs.callPackage ./npd { };
  ptpimg-uploader = pkgs.callPackage ./ptpimg-uploader.nix { };
  rtorrent-exporter = pkgs.callPackage ./rtorrent-exporter.nix { };
  waybar-pd = pkgs.callPackage ./waybar-pd/package.nix { };
  xtor = pkgs.callPackage ./xtor.nix { };
}

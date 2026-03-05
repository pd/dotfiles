{ pkgs, unstable, ... }:
{
  dns-blocklist = pkgs.callPackage ./dns-blocklist.nix {
    inherit (unstable) stevenblack-blocklist;
  };

  filebotd = pkgs.callPackage ./filebotd { };
  launcher = pkgs.callPackage ./launcher { };
  mediaman = pkgs.callPackage ./mediaman { };
  npd = pkgs.callPackage ./npd { };
  ptpimg-uploader = pkgs.callPackage ./ptpimg-uploader.nix { };
  qbt-hooks = pkgs.callPackage ./qbt-hooks { };
  screenshots = pkgs.callPackage ./screenshots.nix { };
  waybar-pd = pkgs.callPackage ./waybar-pd/package.nix { inherit (unstable) zig; };
  xtor = pkgs.callPackage ./xtor.nix { };
}

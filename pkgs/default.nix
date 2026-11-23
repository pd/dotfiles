{
  pkgs,
  pd,
  stevenblack-blocklist,
  hagezi-lgwebos,
  hagezi-pro-plus,
  ...
}:
{
  dns-blocklist = pkgs.callPackage ./dns-blocklist.nix {
    inherit stevenblack-blocklist hagezi-lgwebos hagezi-pro-plus;
  };

  filebotd = pkgs.callPackage ./filebotd { };
  launcher = pkgs.callPackage ./launcher { };
  mediaman = pkgs.callPackage ./mediaman { };
  npd = pkgs.callPackage ./npd { };
  ptpimg-uploader = pkgs.callPackage ./ptpimg-uploader.nix { };
  qbt-hooks = pkgs.callPackage ./qbt-hooks { };
  screenshots = pkgs.callPackage ./screenshots.nix { };
  sonarqube-cli = pkgs.callPackage ./sonarqube-cli.nix { };
  wake-desk = pkgs.callPackage ./wake-desk.nix {
    macs = with pd.net.hosts; pkgs.lib.attrValues desk.macs ++ pkgs.lib.attrValues win.macs;
  };
  waybar-pd = pkgs.callPackage ./waybar-pd/package.nix { };
  xtor = pkgs.callPackage ./xtor.nix { };
}

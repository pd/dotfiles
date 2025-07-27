{
  lib,
  config,
  pd,
  ...
}:
let
  # undefined host will just result in both lan and wg being off,
  # being excluded from default ssh configs, etc.
  host = pd.net.hosts."${config.networking.hostName}" or { };
in
{
  imports = [
    ./lan.nix
    ./wg.nix
  ];

  lan.enable = lib.mkDefault (host ? "lan");
  lan.ipv4 = lib.mkIf config.lan.enable host.lan.ipv4;

  wg.enable = lib.mkDefault (host ? "wg");
  wg.ipv4 = lib.mkIf config.wg.enable host.wg.ipv4;
  wg.ipv6 = lib.mkIf config.wg.enable host.wg.ipv6;
  wg.publicKey = lib.mkIf config.wg.enable host.wg.publicKey;
}

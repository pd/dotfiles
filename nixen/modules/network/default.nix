{ lib, config, ... }:
let
  net = import ../net.nix;
  host = net.hosts."${config.networking.hostName}";
in
{
  imports = [
    ./lan.nix
    ./wan.nix
  ];

  lan.enable = lib.mkDefault (host ? "lan");
  lan.ipv4 = lib.mkIf config.lan.enable host.lan.ip;

  wan.enable = lib.mkDefault (host ? "wg0");
  wan.ipv4 = lib.mkIf config.wan.enable host.wg0.ip;
  wan.publicKey = lib.mkIf config.wan.enable host.wg0.publicKey;
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  externalInterface = "ens3"; # TODO: how to discover or parameterize ens3

  port = 51820;
  net = import ../net.nix;
  wg = net.nets.wg0;
  me = net.hosts."${wg.server}";

  clients = lib.filterAttrs (name: peer: (peer ? "wg0") && name != wg.server) net.hosts;
  peers = lib.mapAttrsToList (name: peer: {
    inherit name;
    allowedIPs = [ "${peer.wg0.ip}/32" ];
    publicKey = peer.wg0.publicKey;
    presharedKeyFile = config.sops.secrets."wireguard-preshared-key-${name}".path;
  }) clients;

  psks = lib.mapAttrs' (name: peer: {
    name = "wireguard-preshared-key-${name}";
    value = {
      mode = "0440";
      owner = config.users.users.root.name;
      group = "wheel";
    };
  }) clients;
in
{
  sops.secrets = {
    wireguard-private-key = {
      mode = "0440";
      owner = config.users.users.root.name;
      group = "wheel";
    };
  } // psks;

  networking = {
    firewall.allowedUDPPorts = [ port ];
    nat = {
      enable = true;
      externalInterface = externalInterface;
      internalInterfaces = [ "wg0" ];
    };

    wireguard.interfaces.wg0 = {
      listenPort = port;
      ips = [ "${me.wg0.ip}/32" ];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${wg.cidr} -o ${externalInterface} -j MASQUERADE
      '';

      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${wg.cidr} -o ${externalInterface} -j MASQUERADE
      '';

      peers = peers;
    };
  };
}

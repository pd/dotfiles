{
  config,
  lib,
  pkgs,
  ...
} : let
  net = import ./net.nix;
  externalInterface = "ens3"; # TODO: how to discover or parameterize ens3
  psks = lib.mapAttrs' (name: peer: {
    name = "wireguard-preshared-key-${name}";
    value = {
      mode = "0440";
      owner = config.users.users.root.name;
      group = "wheel";
    };
  }) net.clients;
in {
  sops.secrets = {
    wireguard-private-key = {
      mode = "0440";
      owner = config.users.users.root.name;
      group = "wheel";
    };
  } // psks;

  networking = {
    firewall.allowedUDPPorts = [net.port];
    nat = {
      enable = true;
      externalInterface = externalInterface;
      internalInterfaces = ["wg0"];
    };

    wireguard.interfaces.wg0 = {
      listenPort = net.port;
      ips = [net.server.ip];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${net.cidr} -o ${externalInterface} -j MASQUERADE
      '';

      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${net.cidr} -o ${externalInterface} -j MASQUERADE
      '';

      peers = lib.mapAttrsToList
        (name: peer: {
          inherit name;
          allowedIPs = [peer.ip];
          publicKey = peer.publicKey;
          presharedKeyFile = config.sops.secrets."wireguard-preshared-key-${name}".path;
        })
        net.clients;
    };
  };
}

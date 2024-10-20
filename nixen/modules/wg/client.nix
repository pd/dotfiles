{
  lib,
  config,
  ...
} : let
  net = import ./net.nix;
  client = net.clients."${config.networking.hostName}";
in {
  sops.secrets.wireguard-private-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  networking = {
    firewall.allowedUDPPorts = [net.port];
    wireguard.interfaces.wg0 = {
      listenPort = net.port;
      ips = [client.ip];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      peers = [
        {
          endpoint = "${net.hostname}:${builtins.toString net.port}";
          publicKey = net.server.publicKey;
          allowedIPs = [net.cidr];
          persistentKeepalive = 25;
        }
      ];
    };
  };

}

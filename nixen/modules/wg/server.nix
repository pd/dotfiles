{
  config,
  ...
} : let
  net = import ./net.nix;
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
      ips = [net.cidr];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      peers = builtins.map (c: {
        publicKey = c.publicKey;
        allowedIPs = [c.ip];
      }) (builtins.filter (c: c ? "publicKey") net.clients);
    };
  };
}

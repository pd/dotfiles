{
  lib,
  config,
  ...
}:
let
  port = 51820;
  net = import ../net.nix;
  wg  = net.nets.wg0;
  srv = net.hosts."${wg.server}";
  me  = net.hosts."${config.networking.hostName}";
in
{
  sops.secrets.wireguard-private-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  sops.secrets.wireguard-preshared-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  networking = {
    firewall.allowedUDPPorts = [ port ];
    wireguard.interfaces.wg0 = {
      listenPort = port; # TODO: port+N ?
      ips = [ me.wg0.ip ];
      privateKeyFile = config.sops.secrets.wireguard-private-key.path;

      peers = [
        {
          endpoint = wg.endpoint;
          allowedIPs = [ wg.cidr ];
          publicKey = srv.publicKey;
          presharedKeyFile = config.sops.secrets.wireguard-preshared-key.path;
          persistentKeepalive = 25;
        }
      ];
    };
  };

}

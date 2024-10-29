{
  config,
  ...
}:
{
  sops.secrets.spanland-private-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  sops.secrets.spanland-preshared-key = {
    mode = "0440";
    owner = config.users.users.root.name;
    group = "wheel";
  };

  networking = {
    firewall.allowedUDPPorts = [ 61820 ];
    networkmanager.unmanaged = [ "wg1" ];
    wireguard.interfaces.wg1 = {
      listenPort = 61820;
      ips = [ "172.30.30.2/32" ];
      privateKeyFile = config.sops.secrets.spanland-private-key.path;

      peers = [{
        endpoint = "wireguard.spantree.net:51820";
        publicKey = "cHPm1u0SA6G2Y2tCDjM1WebvuS3bfRYz++VxPqiQAl8=";
        presharedKeyFile = config.sops.secrets.spanland-preshared-key.path;
        persistentKeepalive = 25;
        allowedIPs = [
          "172.30.0.0/16"
          "10.179.0.0/16"
          "243.132.0.0/14"
          "243.136.0.0/20"
        ];
      }];
    };
  };
}

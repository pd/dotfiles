{
  config,
  pd,
  ...
}:
let
  inherit (pd.net) lan wg;
in
{
  age.secrets.wireguard-private-key.file = ./secrets/wireguard-private-key.age;
  age.secrets.wireguard-preshared-key.file = ./secrets/wireguard-preshared-key.age;

  networking.wg-quick.interfaces.wg0 =
    let
      secrets = config.age.secrets;
    in
    {
      autostart = false;
      privateKeyFile = secrets.wireguard-private-key.path;
      address = [
        "${wg.ipv4.armspan}/32"
        "${wg.ipv6.armspan}/128"
      ];
      dns = [ "${wg.ipv4.pi},${wg.ipv6.pi},wg,home" ];
      postDown = "networksetup -setdnsservers Wi-Fi Empty";

      peers = [
        {
          endpoint = "wg.krh.me:51930";
          publicKey = wg.pks.pi;
          presharedKeyFile = secrets.wireguard-preshared-key.path;
          allowedIPs = [
            lan.cidr
            lan.cidr6
            wg.cidr
            wg.cidr6
          ];
          persistentKeepalive = 25;
        }
      ];
    };
}

{
  config,
  pd,
  pkgs,
  ...
}:
let
  inherit (pd.net) lan wg;
in
{
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/Users/pd/Library/Application Support/sops/age/keys.txt";
  sops.secrets.wireguard-private-key = { };
  sops.secrets.wireguard-preshared-key = { };

  networking.wg-quick.interfaces.wg0 =
    let
      secrets = config.sops.secrets;
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

  # assign the static IPv6 I want iff we're on the right network.
  # the openwrt side is correctly configured to assign these via
  # dhcpv6, mac _supports_ dhcpv6, but then just ignores the IA_NA
  # assignment for the ULA.
  launchd.agents.homeULA =
    let
      dev = "en0";
      ula = lan.ipv6.armspan;

      home-ula-toggle = pkgs.writeShellScript "home-ula-toggle" ''
        on_home_wifi() {
          # macos 15 sequoia has intentionally made it a nightmare
          # to retrieve the ssid you're connected to, so just check
          # that we've probably got a v6 addr in the right /64.
          #
          # why do i still run an OS that is actively hostile
          ifconfig en0 inet6 | grep -q 'fded:40::'
        }

        has_ula() {
          ifconfig ${dev} inet6 | grep -q "${ula}"
        }

        if ! on_home_wifi; then
          ifconfig ${dev} inet6 ${ula} -alias
        elif ! has_ula; then
          ifconfig ${dev} inet6 ${ula} prefixlen 64 alias
        fi
      '';
    in
    {
      command = home-ula-toggle;
      serviceConfig = {
        Label = "home.ula.toggle";
        RunAtLoad = true;

        WatchPaths = [
          "/Library/Preferences/SystemConfiguration/NetworkInterfaces.plist"
          "/Library/Preferences/SystemConfiguration/com.apple.airport.preferences.plist"
        ];
      };
    };
}

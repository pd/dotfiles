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
  networking.wg-quick.interfaces.wg0 =
    let
      secrets = config.home-manager.users.pd.sops.secrets;
    in
    {
      autostart = false;
      privateKeyFile = secrets.wireguard-private-key.path;
      address = [
        "${wg.ipv4.span}/32"
        "${wg.ipv6.span}/128"
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
      ula = lan.ipv6.span;
      ssid = "bazqux"; # TODO: prolly encode ssid somewhere else

      home-ula-toggle = pkgs.writeShellScript "home-ula-toggle" ''
        on_home_wifi() {
          ssid="$(networksetup -getairportnetwork ${dev} | awk -F': ' '{print $2}')"
          test "$ssid" == "${ssid}"
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

        # _something_ in here changes when bouncing across wifi networks,
        # so this is probably running too often but seems to work
        WatchPaths = [ "/Library/Preferences/SystemConfiguration" ];
      };
    };
}

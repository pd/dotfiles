{
  config,
  net,
  pkgs,
  ...
}:
{
  networking.wg-quick.interfaces.wg0 =
    let
      # TODO: dumb to have to reach into home-manager for this
      secrets = config.home-manager.users.pd.sops.secrets;
    in
    {
      autostart = false;
      privateKeyFile = secrets.wireguard-private-key.path;
      address = [
        "${net.wg.ipv4.span}/32"
        "${net.wg.ipv6.span}/128"
      ];
      dns = [ "${net.wg.ipv4.pi},${net.wg.ipv6.pi},wg,home" ];
      postDown = "networksetup -setdnsservers Wi-Fi Empty";

      peers = [
        {
          endpoint = "wg.krh.me:51930";
          publicKey = net.wg.pks.pi;
          presharedKeyFile = secrets.wireguard-preshared-key.path;
          allowedIPs = [
            net.lan.cidr
            net.lan.cidr6
            net.wg.cidr
            net.wg.cidr6
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
      ula = net.lan.ipv6.span;
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

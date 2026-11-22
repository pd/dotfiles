{
  config,
  lib,
  pd,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.networking.homeULA;
in
{
  options.networking.homeULA = {
    enable = mkEnableOption "set static IPv6 because mac ignores the IA_NA assignment for ULA";
    interface = mkOption {
      type = types.nonEmptyStr;
    };
  };

  config = mkIf cfg.enable {
    launchd.daemons.homeULA = {
      command =
        let
          dev = cfg.interface;
          ula = pd.net.lan.ipv6.${config.networking.hostName};
          prefix = "fded:40::";
        in
        pkgs.writeShellScript "home-ula" ''
          on_home_network() {
            ifconfig ${dev} inet6 | grep '${prefix}' | grep -q autoconf
          }

          has_ula() {
            ifconfig  ${dev} inet6 | grep -qw '${ula}'
          }

          if ! on_home_network; then
            ifconfig ${dev} inet6 ${ula} -alias
          elif ! has_ula; then
            ifconfig ${dev} inet6 ${ula} prefixlen 64 alias
          fi
        '';

      serviceConfig = {
        Label = "home.ula.toggle";
        RunAtLoad = true;
        WatchPaths = [
          "/Library/Preferences/SystemConfiguration/NetworkInterfaces.plist"
          "/Library/Preferences/SystemConfiguration/com.apple.airport.preferences.plist"
        ];
      };
    };
  };
}

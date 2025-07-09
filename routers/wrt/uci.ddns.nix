{ lib, ... }:
let
  records = {
    home_a = {
      subdomain = "home";
      id = 1758420910;
    };

    home4 = {
      subdomain = "home4";
      id = 1759037261;
    };

    home_aaaa = {
      subdomain = "home";
      ipv = 6;
      id = 1758421054;
    };

    home6 = {
      subdomain = "home6";
      ipv = 6;
      id = 1759037273;
    };

    wg_a = {
      subdomain = "wg";
      id = 1756936809;
    };

    wg_aaaa = {
      subdomain = "wg";
      ipv = 6;
      id = 1759484529;
      script = "/usr/lib/ddns/update_wg_pi.sh";
    };
  };

  ddnsDO =
    name:
    _@{
      subdomain,
      id,
      ipv ? 4,
      script ? null,
    }:
    {
      enabled = true;
      service_name = "digitalocean.com-v2";
      password._secret = "do_api_key";
      domain = "krh.me";
      username = subdomain;
      lookup_host = "${subdomain}.krh.me";
      param_opt = id;

      use_ipv6 = ipv == 6;
      ip_source = if script != null then "script" else "network";
      ip_network = if ipv == 6 then "wan6" else "wan";
      interface = if ipv == 6 then "wan6" else "wan";

      use_syslog = 2;
      check_unit = "minutes";
      force_unit = "minutes";
      retry_unit = "seconds";
    }
    // (if script != null then { ip_script = script; } else { });
in
{
  global = [
    {
      ddns_dateformat = "%F %R";
      ddns_loglines = 250;
      ddns_rundir = "/var/run/ddns";
      ddns_logdir = "/var/log/ddns";
    }
  ];

  service = lib.mapAttrs ddnsDO records;
}

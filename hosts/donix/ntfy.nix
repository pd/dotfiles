{
  config,
  pd,
  pkgs,
  ...
}:
let
  port = 2600;
  metricsPort = 9712;
  dataDir = "/var/lib/ntfy-sh";
in
{
  networking.firewall.allowedTCPPorts = [ metricsPort ];

  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "https://ntfy.krh.me";
      listen-http = "localhost:${toString port}";
      behind-proxy = true;
      web-root = "disable";

      auth-file = "${dataDir}/auth.db";
      enable-login = true;
      enable-signup = false;
      auth-default-access = "deny-all";

      cache-file = "${dataDir}/cache-file.db";

      log-format = "json";
      enable-metrics = true;
      metrics-listen-http = "${pd.net.wg.ipv4.donix}:${toString metricsPort}";
    };
  };

  sops.secrets."ntfy-acl.env" = { };

  systemd.services.ntfy-acl = {
    description = "Configure ntfy ACLs";
    wantedBy = [ "ntfy-sh.service" ];

    serviceConfig = {
      RemainAfterExit = true;
      EnvironmentFile = config.sops.secrets."ntfy-acl.env".path;
    };

    path = [ pkgs.ntfy-sh ];
    script = ''
      NTFY_PASSWORD="$NTFY_PASSWORD_PUB" ntfy user add --ignore-exists pub
      ntfy access pub '*' wo

      NTFY_PASSWORD="$NTFY_PASSWORD_SUB" ntfy user add --ignore-exists sub
      ntfy access sub '*' ro
    '';
  };

  services.caddy.virtualHosts."ntfy.krh.me" = {
    extraConfig = ''
      reverse_proxy localhost:${toString port}
    '';
  };
}

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

  sops.secrets."ntfy-auth.env" = { };
  services.ntfy-sh = {
    enable = true;
    environmentFile = config.sops.secrets."ntfy-auth.env".path;
    settings = {
      base-url = "https://ntfy.krh.me";
      listen-http = "localhost:${toString port}";
      behind-proxy = true;
      web-root = "disable";

      cache-file = "${dataDir}/cache-file.db";

      log-format = "json";
      enable-metrics = true;
      metrics-listen-http = "${pd.net.wg.ipv4.donix}:${toString metricsPort}";

      auth-file = "${dataDir}/auth.db";
      enable-login = true;
      enable-signup = false;
      auth-default-access = "deny-all";
      auth-users = [
        "pub:$2a$10$jEnModHnG73iwB72BjIdUO3GIbu5N7rO5BZW.Oh8AwdBdQvwq9llC:user"
        "sub:$2a$10$mrRi18t/z9eGAGhZB5SJTea3frlkVbBPtlVo17W9Jg7CiPIk8C3oK:user"
      ];
      auth-access = [
        "pub:*:wo"
        "sub:*:ro"
      ];
    };
  };
}

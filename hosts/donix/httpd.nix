{ config, pd, ... }:
{
  imports = [ ../../modules/httpd ];

  httpd = {
    enable = true;
    metrics.cidrs = with pd.net.wg; [
      cidr
      cidr6
    ];

    staticSites."internetsfamo.us" = {
      root = ./internetsfamous;
      extraConfig = ''
        handle_path /drop/* {
          root /var/lib/internetsfamous/drop
          file_server
        }
      '';
    };
    reverseProxies."ntfy.krh.me" = config.services.ntfy-sh.settings.listen-http;
    reverseProxies."status.krh.me" = "localhost:${toString config.services.gatus.settings.web.port}";
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/internetsfamous/drop 0775 caddy caddy -"
  ];
}

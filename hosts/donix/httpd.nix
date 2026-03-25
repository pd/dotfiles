{ config, pd, ... }:
let
  wg = with pd.net.wg; [
    cidr
    cidr6
  ];
in
{
  imports = [ ../../modules/httpd ];

  httpd = {
    enable = true;
    metrics.cidrs = wg;

    staticSites."internetsfamo.us".root = ./internetsfamous;
    reverseProxies."ntfy.krh.me" = config.services.ntfy-sh.settings.listen-http;
    reverseProxies."gatus.wg:80" = {
      handlers = [
        {
          route = "";
          upstream = "localhost:${toString config.services.gatus.settings.web.port}";
        }
      ];
      cidrs = wg;
    };
  };

}

{ config, pd, ... }:
{
  imports = [ ../../modules/httpd ];

  httpd = {
    enable = true;
    metrics.cidrs = with pd.net.wg; [
      cidr
      cidr6
    ];

    staticSites."internetsfamo.us".root = ./internetsfamous;
    reverseProxies."ntfy.krh.me" = config.services.ntfy-sh.settings.listen-http;
  };

}

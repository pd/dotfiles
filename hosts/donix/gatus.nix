{ config, ... }:
{
  sops.secrets."gatus.env" = { };
  services.gatus = {
    enable = true;
    environmentFile = config.sops.secrets."gatus.env".path;

    settings = {
      web.port = 8989;

      storage = {
        type = "sqlite";
        path = "/var/lib/gatus/gatus.db";
      };

      connectivity.checker = {
        target = "1.1.1.1:53";
        interval = "300s";
      };

      alerting.ntfy = {
        url = "https://ntfy.krh.me";
        topic = "lab";
        token = "$NTFY_ACCESS_TOKEN_PUB";
      };

      endpoints =
        let
          to-ntfy = [
            {
              type = "ntfy";
              failure-threshold = 2;
              success-threshold = 1;
              send-on-resolved = true;
            }
          ];
        in
        [
          {
            name = "jf";
            url = "https://jf.krh.me/web/";
            interval = "60s";
            conditions = [ "[STATUS] == 200" ];
            alerts = to-ntfy;
          }

          {
            name = "npd";
            url = "https://npd.krh.me";
            interval = "60s";
            conditions = [ "[STATUS] == 200" ];
            alerts = to-ntfy;
          }
        ];
    };
  };
}

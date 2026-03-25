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

      security.oidc = {
        issuer-url = "https://accounts.google.com";
        redirect-url = "https://status.krh.me/authorization-code/callback";
        client-id = "$GOOGLE_OIDC_CLIENT_ID";
        client-secret = "$GOOGLE_OIDC_CLIENT_SECRET";
        scopes = [ "openid" ];
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
            interval = "120s";
            conditions = [ "[STATUS] == 200" ];
            alerts = to-ntfy;
          }

          {
            name = "npd";
            url = "https://npd.krh.me";
            interval = "300s";
            conditions = [ "[STATUS] == 200" ];
            alerts = to-ntfy;
          }
        ];
    };
  };
}

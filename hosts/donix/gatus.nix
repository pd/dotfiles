{ config, pkgs, ... }:
let
  ntfy = attrs: [
    (
      {
        type = "ntfy";
        send-on-resolved = true;
      }
      // attrs
    )
  ];
in
{
  sops.secrets."gatus.env" = { };
  services.gatus = {
    enable = true;
    package = pkgs.unstable.gatus;
    environmentFile = config.sops.secrets."gatus.env".path;

    settings = {
      web.port = 8989;

      ui = {
        title = "despotia";
        header = "despotia";
        description = " ";
        dashboard-heading = "whats up";
        dashboard-subheading = " ";
      };

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

      external-endpoints =
        let
          to-ntfy = ntfy {
            failure-threshold = 1;
            success-threshold = 1;
          };

          heartbeat = name: {
            inherit name;
            group = "heartbeat";
            token = "$GATUS_HEARTBEAT_TOKEN";
            heartbeat.interval = "5m";
            alerts = to-ntfy;
          };
        in
        [
          (heartbeat "pi")
          (heartbeat "htpc")
        ];

      endpoints =
        let
          to-ntfy = ntfy {
            failure-threshold = 2;
            success-threshold = 1;
          };
        in
        [
          {
            name = "jf";
            group = "svc";
            url = "https://jf.krh.me/web/";
            interval = "120s";
            alerts = to-ntfy;
            conditions = [ "[STATUS] == 200" ];
          }

          {
            name = "rr";
            group = "svc";
            url = "https://rr.krh.me/login";
            interval = "120s";
            alerts = to-ntfy;
            conditions = [ "[STATUS] == 200" ];
          }

          {
            name = "npd";
            group = "svc";
            url = "https://npd.krh.me/favicon.svg";
            interval = "300s";
            conditions = [ "[STATUS] == 200" ];
            alerts = to-ntfy;
          }

          {
            name = "ntfy";
            group = "svc";
            url = "https://ntfy.krh.me/v1/health";
            interval = "300s";
            conditions = [
              "[STATUS] == 200"
              "[BODY].healthy == true"
            ];
            alerts = to-ntfy;
          }

          {
            name = "internetsfamous";
            group = "svc";
            url = "https://internetsfamo.us";
            interval = "600s";
            conditions = [
              "[STATUS] == 200"
              "[BODY] == pat(*a fine town*)"
            ];
            alerts = to-ntfy;
          }
        ];
    };
  };
}

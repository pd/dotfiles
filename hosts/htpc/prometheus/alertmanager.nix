{ config, ... }:
let
  ports.alertmanager-ntfy = "18888";
in
{
  services.prometheus.alertmanagers = [
    {
      scheme = "http";
      static_configs = [ { targets = [ "alerts.home" ]; } ];
    }
  ];

  services.prometheus.alertmanager = {
    enable = true;
    webExternalUrl = "http://alerts.home";

    configuration = {
      receivers = [
        {
          name = "ntfy";
          webhook_configs = [
            { url = "http://127.0.0.1:${ports.alertmanager-ntfy}/hook"; }
          ];
        }
      ];
      route.receiver = "ntfy";
    };
  };

  sops.secrets.alertmanager-ntfy = { };
  services.prometheus.alertmanager-ntfy = {
    enable = true;
    settings = {
      http.addr = "127.0.0.1:${ports.alertmanager-ntfy}";
      ntfy.baseurl = "https://ntfy.krh.me";
      ntfy.notification = {
        topic = "lab";
        priority = "default";
        templates = {
          title = ''{{ if eq .Status "resolved" }}Resolved: {{ end }}{{ index .Annotations "summary" }}'';
          description = ''{{ index .Annotations "description" }}'';
        };
      };
    };
    extraConfigFiles = [ config.sops.secrets.alertmanager-ntfy.path ];
  };
}

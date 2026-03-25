{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  beat = pkgs.writeShellApplication {
    name = "heartbeat";
    runtimeInputs = [ pkgs.curl ];
    text = builtins.readFile ./beat.sh;
  };

  cfg = config.monitoring;
in
{
  options.monitoring.heartbeat = mkEnableOption "send heartbeat to gatus";

  config = mkIf cfg.heartbeat {
    sops.secrets."gatus-heartbeat.env" = { };

    systemd.services.heartbeat = {
      description = "Send heartbeat to gatus";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        EnvironmentFile = config.sops.secrets."gatus-heartbeat.env".path;
        ExecStart = "${beat}/bin/heartbeat";
      };
    };

    systemd.timers.heartbeat = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "2min";
      };
    };
  };
}

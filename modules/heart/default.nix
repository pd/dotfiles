{
  config,
  lib,
  pkgs,
  ...
}:
let
  beat = pkgs.writeShellApplication {
    name = "heartbeat";
    runtimeInputs = [ pkgs.curl ];
    text = builtins.readFile ./beat.sh;
  };

  monitor = pkgs.writeShellApplication {
    name = "heart-monitor";
    runtimeInputs = [ pkgs.curl pkgs.gnugrep ];
    text = builtins.readFile ./monitor.sh;
  };
in
{
  options.heart = {
    beat = lib.mkEnableOption "send heartbeat to ntfy";
    monitor = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "monitor ntfy topics for other hosts heartbeats";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf config.heart.beat {
      sops.secrets."ntfy-heartbeat.env" = { };

      systemd.services.heartbeat = {
        description = "Send heartbeat to ntfy";
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        serviceConfig = {
          Type = "oneshot";
          EnvironmentFile = config.sops.secrets."ntfy-heartbeat.env".path;
          ExecStart = "${beat}/bin/heartbeat";
        };
      };

      systemd.timers.heartbeat = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "1min";
          OnUnitActiveSec = "5min";
        };
      };
    })

    (lib.mkIf (config.heart.monitor != [ ]) {
      sops.secrets."ntfy-acl.env" = { };

      systemd.services.heart-monitor = {
        description = "Monitor heartbeats";
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        serviceConfig = {
          Type = "oneshot";
          EnvironmentFile = config.sops.secrets."ntfy-acl.env".path;
          StateDirectory = "heart-monitor";
          ExecStart = "${monitor}/bin/heart-monitor ${lib.concatStringsSep " " config.heart.monitor}";
        };
      };

      systemd.timers.heart-monitor = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "2min";
          OnUnitActiveSec = "5min";
        };
      };
    })
  ];
}

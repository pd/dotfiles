{
  config,
  pkgs,
  ...
}:
let
  echoserver = pkgs.buildGoModule {
    name = "echoserver";
    src = ./echoserver;
    vendorHash = null;
    CGO_ENABLED = 0;
  };
in
{
  networking.firewall = {
    allowedTCPPorts = [ 1025 ];
    allowedUDPPorts = [ 1025 ];
  };

  systemd.services.spanland-echo = {
    enable = true;
    description = "spanland echoserver";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "exec";
      User = config.users.users.pd.name;
      ExecStart = "${echoserver}/bin/echoserver";
    };
  };
}

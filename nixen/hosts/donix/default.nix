{
  lib,
  pkgs,
  config,
  ...
}:
let
  internetsfamous = pkgs.runCommand "install-nginx-site" { } ''
    install -Dm644 ${./internetsfamous/index.html} $out/www/index.html
  '';

  cnames = host: host.cnames or [];

  net = import ../../modules/net.nix;
  wanHosts = lib.filterAttrs (_: peer: peer ? "wg0") net.hosts;
  records = lib.flatten (
    lib.mapAttrsToList (
      name: host:
      [ ''"${name}.home. IN A ${host.wg0.ip}"'' ]
      ++ builtins.map (cname: ''"${cname}.home. IN CNAME ${name}.home."'') (cnames host)
    ) wanHosts
  );

in
{
  imports = [
    ../../modules/base.nix
    ../../modules/wg/server.nix
    ./users/pd.nix
    ./users/rhys.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "donix";
  networking.firewall = {
    allowedTCPPorts = [
      53
      80
      443
    ];
    allowedUDPPorts = [ 53 ];
  };

  time.timeZone = "America/Chicago";

  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [
          "0.0.0.0"
          "::1"
        ];
        access-control = [
          "127.0.0.1/8 allow"
          "10.100.100.0/24 allow"
        ];

        local-zone = [ "home. static" ];
        local-data = records;
      };

      forward-zone = [
        {
          name = ".";
          forward-addr = [
            "1.1.1.1"
            "8.8.8.8"
          ];
        }
      ];
    };
  };

  services.nginx = {
    enable = true;
    statusPage = false;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedBrotliSettings = true;
    recommendedGzipSettings = true;
    recommendedZstdSettings = true;

    virtualHosts."donix.krh.me" = {
      enableACME = true;
      forceSSL = true;
      root = "${internetsfamous}/www";
    };
  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "letsencrypt@krh.me";

  environment.systemPackages = [ internetsfamous ];
}

{
  pkgs,
  config,
  ...
}:
let
  internetsfamous = pkgs.runCommand "install-nginx-site" { } ''
    install -Dm644 ${./internetsfamous/index.html} $out/www/index.html
  '';

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
    allowedTCPPorts = [ 53 80 443 ];
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
        local-data = [ # TODO: lift from network defn
          ''"donix.home. IN A 10.100.100.1"''
          ''"desk.home. IN A 10.100.100.10"''
          ''"span.home. IN A 10.100.100.11"''
        ];
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

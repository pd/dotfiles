{
  pkgs,
  ...
}:
let
  internetsfamous = pkgs.runCommand "install-nginx-site" { } ''
    install -Dm644 ${./internetsfamous/index.html} $out/www/index.html
  '';
in
{
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "letsencrypt@krh.me";

  environment.systemPackages = [ internetsfamous ];

  services.nginx = {
    enable = true;
    statusPage = false;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedBrotliSettings = true;
    recommendedGzipSettings = true;
    recommendedZstdSettings = true;

    virtualHosts."internetsfamo.us" = {
      enableACME = true;
      forceSSL = true;
      root = "${internetsfamous}/www";
    };
  };
}

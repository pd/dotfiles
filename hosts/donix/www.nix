{ net, pkgs, ... }:
let
  internetsfamous = pkgs.runCommand "internetsfamous-site" { } ''
    install -Dm644 ${./internetsfamous/index.html} $out/www/index.html
  '';
in
{
  networking.firewall.allowedTCPPorts = [
    80
    443
    2020
  ];

  environment.systemPackages = [ internetsfamous ];

  services.caddy = {
    enable = true;
    email = "letsencrypt@krh.me";

    globalConfig = ''
      metrics { per_host }
    '';

    virtualHosts."internetsfamo.us".extraConfig = ''
      root ${internetsfamous}/www
      file_server
    '';

    virtualHosts.":2020".extraConfig = ''
      @wan not client_ip ${net.wg.cidr} ${net.wg.cidr6}
      abort @wan
      metrics
    '';
  };
}

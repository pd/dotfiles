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
  ];

  system.stateVersion = "24.05";

  networking.hostName = "donix";
  networking.firewall = {
    allowedTCPPorts = [
      80
      443
    ];
  };

  time.timeZone = "America/Chicago";

  users.users = {
    rhys = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "wheel"
        "keys"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOZGcHggrgVlMOSh2lG3i8Jp1vA2rz7NyuWSnlVYnUh"
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

{
  config,
  net,
  pkgs,
  ...
}:
let
  pd = config.users.users.pd;
  nasIP = net.hosts.nas.lan.ip;

  filebotd = pkgs.buildGoModule {
    name = "filebotd";
    src = ./filebotd;
    vendorHash = null;
    CGO_ENABLED = 0;
  };

  rtorrent-exporter = pkgs.stdenv.mkDerivation {
    name = "rtorrent-exporter";
    src = pkgs.fetchurl {
      url = "https://github.com/thde/rtorrent_exporter/releases/download/v1.3.2/rtorrent_exporter_1.3.2_linux_amd64.tar.gz";
      sha256 = "24psJpXnKniTHg3BqXoVyazwBHeb75r7F7/LX3tC0SY=";
    };
    phases = [
      "unpackPhase"
      "installPhase"
    ];
    sourceRoot = ".";
    installPhase = ''
      mkdir -p $out/bin
      cp rtorrent_exporter $out/bin
    '';
  };
in
{
  sops.secrets.filebot-license = {
    path = "${pd.home}/.local/share/filebot/data/.license";
    owner = pd.name;
    group = pd.group;
  };

  sops.secrets.jellyfin-api-key = {
    owner = pd.name;
    group = pd.group;
  };

  environment.systemPackages = with pkgs; [
    filebot
    filebotd
    nfs-utils
    rtorrent-exporter
  ];

  # TODO: figure out why the ordering of the automount
  # means dns resolution isn't functioning yet
  fileSystems."/media" = {
    fsType = "nfs";
    device = "${nasIP}:/volume1/media";
    options = [
      "noatime"
      "x-systemd.automount"
    ];
  };

  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      User = pd.name;
      ExecStart = "${filebotd}/bin/filebotd";
      EnvironmentFile = config.sops.secrets.jellyfin-api-key.path;

      # TODO: lib.makeBinPath [pkgs.filebot] is insufficient,
      # cuz it then shells out to uname, dirname, etc. not sure
      # how to generate the "transitively required PATH" or
      # even what that's called in nixland
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };

  # TODO: is there some way to piggyback on the existing
  # prometheus exporter tooling to generate all this?
  systemd.services.rtorrent-exporter = {
    enable = true;
    description = "rtorrent-exporter";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      ExecStart = ''
        ${rtorrent-exporter}/bin/rtorrent_exporter \
          --rtorrent.scrape-uri="http://nas.home:8000/RPC2"
      '';

      # TODO builtins get their own user
      User = config.users.users.prometheus.name;
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;

    virtualHosts."store.home".locations."/" = {
      proxyPass = "http://nas.home:5000";
    };

    virtualHosts."torrent.home" = {
      locations."/_hooks/" = {
        proxyPass = "http://127.0.0.1:12345";
      };

      locations."/" = {
        proxyPass = "http://nas.home:8080";
      };
    };
  };

}

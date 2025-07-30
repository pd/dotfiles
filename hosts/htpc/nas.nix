{
  config,
  inputs,
  pkgs,
  ...
}:
let
  pd = config.users.users.pd;

  filebot = pkgs.filebot.overrideAttrs (
    final: prev: {
      # https://github.com/NixOS/nixpkgs/pull/429413
      installPhase = with pkgs; ''
        mkdir -p $out/opt $out/bin
        # Since FileBot has dependencies on relative paths between files, all required files are copied to the same location as is.
        cp -r filebot.sh lib/ jar/ $out/opt/
        cp -r filebot.sh jar/ $out/opt/
        # Copy lib based on platform and force filebot to use libmediainfo.so from nix
        local platformDir
        case "${stdenv.hostPlatform.system}" in
          "x86_64-linux")
            platformDir="Linux-x86_64"
            ;;
          "aarch64-linux")
            platformDir="Linux-aarch64"
            ;;
        esac
        if [ -n "$platformDir" ]; then
          mkdir -p "$out/opt/lib"
          cp -r "lib/$platformDir" "$out/opt/lib/"
          rm "$out/opt/lib/$platformDir/libmediainfo.so"
          ln -s "${libmediainfo}/lib/libmediainfo.so" "$out/opt/lib/$platformDir/"
        fi
        # Filebot writes to $APP_DATA, which fails due to read-only filesystem. Using current user .local directory instead.
        substituteInPlace $out/opt/filebot.sh \
          --replace 'APP_DATA="$FILEBOT_HOME/data/$(id -u)"' 'APP_DATA=''${XDG_DATA_HOME:-$HOME/.local/share}/filebot/data' \
          --replace '$FILEBOT_HOME/data/.license' '$APP_DATA/.license' \
          --replace '-jar "$FILEBOT_HOME/jar/filebot.jar"' '-Dcom.googlecode.lanterna.terminal.UnixTerminal.sttyCommand=${coreutils}/bin/stty -jar "$FILEBOT_HOME/jar/filebot.jar"'
        wrapProgram $out/opt/filebot.sh \
          --prefix PATH : ${lib.makeBinPath [ openjdk17 ]}
        # Expose the binary in bin to make runnable.
        ln -s $out/opt/filebot.sh $out/bin/filebot
      '';
    }
  );
in
{
  imports = [
    "${inputs.private}/archival"
  ];

  sops.secrets.filebot-license = {
    path = "${pd.home}/.local/share/filebot/data/.license";
    owner = pd.name;
    group = pd.group;
  };

  sops.secrets."rtorrent-exporter.yaml" = {
    owner = config.users.users.prometheus.name;
  };

  environment.systemPackages =
    [ filebot ]
    ++ (with pkgs; [
      chromaprint # audio id for filebot
      flac
      imagemagick
      intermodal # fancier mktorrent
      nfs-utils
    ])
    ++ (with pkgs.pd; [
      filebotd
      mediaman
      rtorrent-exporter
    ]);

  fileSystems."/media" = {
    fsType = "nfs";
    device = "nas.home:/volume1/media";
    options = [
      "noatime"
      "x-systemd.automount"
      "x-systemd.requires=network-online.target"
    ];
  };

  systemd.services.filebotd = {
    enable = true;
    description = "filebotd";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [ pkgs.filebot ];
    serviceConfig = {
      Type = "exec";
      User = pd.name;
      ExecStart = "${pkgs.pd.filebotd}/bin/filebotd";
    };
  };

  networking.firewall.allowedTCPPorts = [ 9135 ];
  systemd.services.rtorrent-exporter = {
    enable = true;
    description = "rtorrent-exporter";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      Type = "exec";
      User = config.users.users.prometheus.name;
      ExecStart = ''
        ${pkgs.pd.rtorrent-exporter}/bin/rtorrent-exporter \
          --config ${config.sops.secrets."rtorrent-exporter.yaml".path}
      '';
    };
  };

  systemd.services.media-prune = {
    serviceConfig = {
      Type = "oneshot";
      User = "pd";
    };

    script = "${pkgs.pd.mediaman}/bin/media-prune";
  };

  systemd.timers.media-prune = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = [ "60m" ];
      OnUnitActiveSec = [ "720m" ];
    };
  };

  services.caddy.virtualHosts = {
    "store.home:80".extraConfig = ''
      reverse_proxy nas.home:5000
    '';

    "torrent.home:80".extraConfig = ''
      handle /_hooks/* {
        reverse_proxy localhost:12345
      }

      handle {
        reverse_proxy nas.home:8080
      }
    '';
  };
}

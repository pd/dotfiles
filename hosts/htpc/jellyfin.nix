{ pkgs, ... }:
{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-compute-runtime
      intel-media-driver
      intel-media-sdk
      intel-vaapi-driver
      vaapiVdpau
    ];
  };

  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];

  services.caddy.virtualHosts."jellyfin.home:80".extraConfig = ''
    reverse_proxy localhost:8096
  '';

  services.caddy.virtualHosts."jf.krh.me".extraConfig = ''
    reverse_proxy localhost:8096
  '';
}

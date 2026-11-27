{ pkgs, ... }:
{
  services.jellyfin = {
    enable = true;
    package = pkgs.unstable.jellyfin;
  };

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vpl-gpu-rt
    ];
  };
}

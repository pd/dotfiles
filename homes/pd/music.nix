{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    pd.ptpimg-uploader
    pd.xtor
  ];

  sops.secrets.PTPIMG_API_KEY = { };
  sops.secrets.OPS_ANNOUNCE_URL = { };

  programs.zsh.envExtra = ''
    export PTPIMG_API_KEY="$(cat ${config.sops.secrets.PTPIMG_API_KEY.path})"
    export OPS_ANNOUNCE_URL="$(cat ${config.sops.secrets.OPS_ANNOUNCE_URL.path})"
  '';
}

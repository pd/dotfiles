{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    pd.ptpimg-uploader
    pd.xtor
  ];

  age.secrets.PTPIMG_API_KEY.file = ./secrets/PTPIMG_API_KEY.age;
  age.secrets.OPS_ANNOUNCE_URL.file = ./secrets/OPS_ANNOUNCE_URL.age;

  programs.zsh.envExtra = ''
    export PTPIMG_API_KEY="$(cat ${config.age.secrets.PTPIMG_API_KEY.path})"
    export OPS_ANNOUNCE_URL="$(cat ${config.age.secrets.OPS_ANNOUNCE_URL.path})"
  '';
}

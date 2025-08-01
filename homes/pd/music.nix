{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    flac
    imagemagick
    intermodal # for imdl verify mostly
    lame
    mktorrent
    pyrosimple # lstor, rtxmlrpc

    pd.ptpimg-uploader
  ];

  sops.secrets.PTPIMG_API_KEY = { };
  sops.secrets.OPS_ANNOUNCE_URL = { };

  programs.zsh.envExtra = ''
    export PTPIMG_API_KEY="$(cat ${config.sops.secrets.PTPIMG_API_KEY.path})"
    export OPS_ANNOUNCE_URL="$(cat ${config.sops.secrets.OPS_ANNOUNCE_URL.path})"
  '';
}

{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    flac
    imagemagick
    lame
    mktorrent
    pyrosimple # lstor, rtxmlrpc

    pd.ptpimg-uploader
  ];

  sops.secrets.PTPIMG_API_KEY = { };

  programs.zsh.envExtra = ''
    export PTPIMG_API_KEY="$(cat ${config.sops.secrets.PTPIMG_API_KEY.path})"
  '';
}

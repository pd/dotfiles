{
  buildEnv,

  flac,
  ffmpeg_6-headless,
  imagemagick,
  intermodal,
  lame,
  mktorrent,
  pyrosimple,
  qbittorrent-cli,
  unzip,
}:
buildEnv {
  name = "xtor";
  paths = [
    flac # metaflac
    ffmpeg_6-headless # ffprobe
    imagemagick
    intermodal # imdl verify
    lame
    mktorrent
    pyrosimple # lstor
    qbittorrent-cli
    unzip
  ];
}

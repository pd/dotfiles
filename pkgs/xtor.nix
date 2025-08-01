{
  buildEnv,

  flac,
  imagemagick,
  intermodal,
  lame,
  mktorrent,
  pyrosimple,
  unzip,
}:
buildEnv {
  name = "xtor";
  paths = [
    flac # metaflac
    imagemagick
    intermodal # imdl verify
    lame
    mktorrent
    pyrosimple # lstor, rtxmlrpc
    unzip
  ];
}

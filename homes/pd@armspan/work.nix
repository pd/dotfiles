{ pkgs, ... }:
{
  programs.bun = {
    enable = true;
    settings = {
      telemetry = false;
    };
  };

  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
    };
  };

  home.packages = with pkgs; [
    mermaid-cli
    pandoc

    (texliveBasic.withPackages (
      ps: with ps; [
        bookmark
        booktabs
        etoolbox
        fancyhdr
        fancyvrb
        float
        hyperref
        lastpage
        listings
        mdwtools
        microtype
        needspace
        newunicodechar
        parskip
        placeins
        sectsty
        titlesec
        tocloft
        unicode-math
        upquote
        xcolor
        xetex
        xurl
      ]
    ))
  ];

}

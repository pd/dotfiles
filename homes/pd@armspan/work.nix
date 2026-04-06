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
    ansible
    mermaid-cli
    nodejs_24
    pandoc
    sshpass
    tart

    pd.sonarqube-cli

    (python3.withPackages (pp: [
      pp.avro
      pp.boto3
      pp.duckdb
      pp.jinja2
      pp.pandas
      pp.pyarrow
    ]))

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

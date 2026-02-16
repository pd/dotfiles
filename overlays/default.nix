{ inputs, ... }:
{
  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;
    };
  };

  # Add my own packages as `pkgs.pd`
  pd = final: _prev: {
    pd = import ../pkgs {
      pkgs = final;
      unstable = final.unstable;
    };
  };

  # Additional treesitter grammars
  tree-sitter = _final: prev: {
    tree-sitter = prev.tree-sitter.override {
      extraGrammars = {
        tree-sitter-qmljs = prev.tree-sitter.buildGrammar {
          language = "qmljs";
          version = "0889da4";
          src = prev.fetchFromGitHub {
            owner = "yuja";
            repo = "tree-sitter-qmljs";
            rev = "0889da4632bba3ec6f39ef4102625654890c15c1";
            sha256 = "sha256-Twj2taG7xFTPXTvzDcWeIqxUAkuhsybwZvtwK/HiruE=";
          };
        };
      };
    };
  };

  # Roll back inetutils to 2.6. nixos-25.11 updated to 2.7 to fix a telnetd CVE,
  # but who cares, telnetd IS the CVE. inetutils 2.7 doesn't build cleanly on mac
  # and/or clang in general, too lazy to figure it out.
  #
  # upstream:
  # https://github.com/nixos/nixpkgs/issues/488689
  inetutils_26 = final: _prev: {
    inetutils = final.unstable.inetutils;
  };
}

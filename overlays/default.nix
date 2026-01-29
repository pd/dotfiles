{ inputs, ... }:
{
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

  # Expose nixpkgs-unstable as `pkgs.unstable.foo`
  unstable = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;
    };
  };
}

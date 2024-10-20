{ ... } :
{
  home-manager.users.pd = {
    home.stateVersion = "24.05";

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.git = {
      enable = true;
      userName = "Kyle Hargraves";
      userEmail = "pd@krh.me";

      aliases = {
        b = "branch";
        co = "checkout";
        ci = "commit";
        cp = "cherry-pick";
        rb = "rebase";
        st = "status";
      };

      ignores = [
        ".DS_Store"
        "*.swp"
        ".#*"
        "#*#"
        "*~"
      ];

      extraConfig = {
        diff = {
          algorithm = "patience";
        };

        init = {
          defaultBranch = "main";
        };

        push = {
          default = "upstream";
        };

        status = {
          relativePaths = false;
        };

        url."git@github.com" = {
          insteadOf = "https://github.com";
        };
      };
    };

    programs.go.enable = true;

    programs.zsh = {
      enable = true;
      autocd = true;

      # Ensure tramp can parse the prompt
      envExtra = ''
        [[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ '
      '';

      shellAliases = {
        g = "git";
      };
    };

  };
}

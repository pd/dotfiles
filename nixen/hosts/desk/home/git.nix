{
  home-manager.users.pd.programs.git = {
    enable = true;
    userName = "Kyle Hargraves";
    userEmail = "pd@krh.me";

    aliases = {
      append = "commit --amend -C HEAD";
      b = "branch";
      bv = "branch -v";
      co = "checkout";
      ci = "commit";
      cp = "cherry-pick";
      d = "diff";
      ds = "diff --staged";
      l = "log";
      m = "merge";
      pp = "pull --prune";
      rb = "rebase";
      rup = "remote update --prune";
      sh = "show";
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
}

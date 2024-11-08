{ pkgs, ... }:
{
  home-manager.users.pd = {
    programs.nixvim = {
      enable = true;

      extraPackages = with pkgs; [
        gopls # TODO lift to home prolly
        gotools
        stylua
      ];

      globals.mapleader = " ";

      colorschemes.kanagawa = {
        enable = true;
        settings.theme = "dragon";
      };

      plugins = {
        direnv.enable = true;
        lualine.enable = true;
        treesitter.enable = true;
        which-key.enable = true;

        conform-nvim = {
          enable = true;
          formatOnSave = {
            lspFallback = true;
            timeoutMs = 500;
          };
          formattersByFt = {
            go = [ "goimports" ];
            lua = [ "stylua" ];
            nix = [ "nixfmt" ];
            "_" = [ "trim_whitespace" ];
          };
        };

        telescope = {
          enable = true;
          extensions.frecency.enable = true;
          extensions.fzf-native.enable = true;
          keymaps =
            let
              pk = action: desc: {
                inherit action;
                options.desc = desc;
              };
            in
            {
              "<leader>ff" = pk "find_files" "Pick file";
              "<leader>fg" = pk "git_files" "Pick file in git";
              "<leader>fG" = pk "live_grep" "Pick file by grep";
              "<leader>fb" = pk "buffers" "Pick buffer";
            };
        };
      };
    };

    # If we're using home-manager, then favor nvim
    programs.zsh.envExtra = ''
      alias vim=nvim
      alias vimdiff='nvim -d'
    '';
  };
}

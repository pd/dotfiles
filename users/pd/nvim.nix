{ pkgs, ... }:
{
  home-manager.users.pd = {
    programs.nixvim = {
      enable = true;

      # I will not be learning yet another clipboard management system
      clipboard.register = "unnamedplus";
      opts.mouse = "";

      extraPackages = with pkgs; [
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
          settings = {
            format_on_save = {
              lsp_format = "fallback";
              timeout_ms = 500;
            };
            formatters_by_ft = {
              go = [ "goimports" ];
              lua = [ "stylua" ];
              nix = [ "nixfmt" ];
              "_" = [ "trim_whitespace" ];
            };
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

        web-devicons.enable = true;
      };
    };

    programs.zsh.shellAliases = {
      vim = "nvim";
      vimdiff = "nvim -d";
    };
  };
}

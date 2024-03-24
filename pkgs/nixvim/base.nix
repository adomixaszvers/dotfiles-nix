{ pkgs, ... }:
{
  config = {
    wrapRc = true;
    globals = {
      mapleader = " ";
      rainbow_active = 1;
      suda_smart_edit = 0;
      ale_disable_lsp = 1;
      ale_use_neovim_diagnostics_api = 1;
    };

    # Tab specific options
    opts = {
      tabstop = 8; # A tab is 8 spaces
      expandtab = true; # Always uses spaces instead of tabs
      softtabstop = 4; # Insert 4 spaces when tab is pressed
      shiftwidth = 4; # An indent is 4 spaces
      shiftround = true; # Round indent to nearest shiftwidth multiple

      number = true;
      relativenumber = true;
      list = true;
      inccommand = "nosplit"; # Shows the effects of a command incrementally, as you type.
      background = "dark";
      cursorline = true;
      scrolloff = 5; # minimal lines visible above and below cursor

      showmode = false;
      mouse = "";
      updatetime = 100;
      grepprg = "rg --vimgrep --no-heading --smart-case";
      timeout = true;
      timeoutlen = 300;
    };
    keymaps =
      let
        options = {
          noremap = true;
          silent = true;
        };
        mkMap = mode: key: action: {
          inherit
            mode
            key
            action
            options
            ;
        };
      in
      [
        (mkMap "n" "<leader><leader>" "<cmd>Telescope find_files<cr>")
        (mkMap "n" "<leader>/" "<cmd>Telescope live_grep<cr>")
        (mkMap "n" ''<leader>"'' "<cmd>Telescope resume<cr>")
        (mkMap "n" "<leader>," "<cmd>Telescope buffers<cr>")
        (mkMap "n" "<leader>f" "<cmd>Neoformat<cr>")
        (mkMap "n" "-" "-")
        (mkMap "n" "<space>e" "<cmd>lua vim.diagnostic.open_float()<CR>")
        (mkMap "n" "[d" "<cmd>lua vim.diagnostic.goto_prev()<CR>")
        (mkMap "n" "]d" "<cmd>lua vim.diagnostic.goto_next()<CR>")
        (mkMap "n" "<space>q" "<cmd>lua vim.diagnostic.setloclist()<CR>")
      ];
    extraConfigLua = ''
      vim.opt.path:append("**")
      if vim.fn.has("termguicolors") then
          vim.o.termguicolors = true
      end

      if vim.env.TERM ~= "linux" then
          vim.cmd('colorscheme nordic')
          vim.g.airline_powerline_fonts = 1
      else
          vim.cmd('colorscheme solarized')
          vim.g.airline_symbols_ascii = 1
          vim.g.guicursor = '''
      end
    '';
    autoGroups.nix-flakes.clear = true;
    autoCmd = [
      {
        event = [
          "BufNewFile"
          "BufRead"
        ];
        pattern = "flake.lock";
        group = "nix-flakes";
        callback.__raw = ''
          function()
            vim.bo.filetype = 'json'
          end
        '';
      }
    ];
    extraPlugins = with pkgs.vimPlugins; [
      ale
      direnv-vim
      neoformat
      nordic-nvim
      repeat
      solarized
      vim-suda
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
      yuck-vim
    ];
    plugins = {
      airline.enable = true;
      commentary.enable = true;
      fugitive.enable = true;
      gitgutter.enable = true;
      rainbow-delimiters.enable = true;
      telescope.enable = true;
      treesitter.enable = true;
      web-devicons.enable = true;
      which-key.enable = true;
    };
  };
}

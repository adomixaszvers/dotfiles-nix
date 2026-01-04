{ pkgs, ... }:
{
  inherit pkgs;
  module = {
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
        lsp = {
          enable = true;
          servers = {
            lua_ls.enable = true;
            hls = {
              enable = true;
              installGhc = false;
            };
            pylsp.enable = true;
            rust_analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
            };
          };
          onAttach = ''
            local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
            local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

            -- Enable completion triggered by <c-x><c-o>
            buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

            -- Mappings.
            local opts = { noremap = true, silent = true }

            -- See `:help vim.lsp.*` for documentation on any of the below functions
            buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
            buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
            buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
            buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
            buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
            buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
            buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
            buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
            buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
            buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
            buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
            buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
            buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.format({ async = true })<CR>', opts)
          '';
        };
        rainbow-delimiters.enable = true;
        telescope.enable = true;
        treesitter.enable = true;
        web-devicons.enable = true;
        which-key.enable = true;
      };
    };
  };
}

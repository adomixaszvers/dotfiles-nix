require('nvim-treesitter.configs').setup({
  auto_install = false,
  ensure_installed = {},
  ignore_install = {},
  modules = {},
  sync_install = false,
  highlight = {
    enable = true, -- false will disable the whole extension
    -- disable = { "bash" },
    is_supported = function()
      if
        vim.fn.strwidth(vim.fn.getline('.')) > 300
        or vim.fn.getfsize(vim.fn.expand('%')) > 1024 * 1024
      then
        return false
      else
        return true
      end
    end,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<leader>gn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  },
})

require 'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true, -- false will disable the whole extension
        -- disable = { "bash" },
        is_supported = function()
            if vim.fn.strwidth(vim.fn.getline('.')) > 300
                or vim.fn.getfsize(vim.fn.expand('%')) > 1024 * 1024 then
                return false
            else
                return true
            end
        end,
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<leader>gn",
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
        },
    },
}

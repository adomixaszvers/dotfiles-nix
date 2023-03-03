require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,              -- false will disable the whole extension
        disable = { "bash" },
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

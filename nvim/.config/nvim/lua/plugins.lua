vim.pack.add({
    -- File explorer
    { src = "https://github.com/stevearc/oil.nvim" },
    -- Move between files a little better
    { src = "https://github.com/vieitesss/miniharp.nvim" },
    -- Fuzzy find things
    { src = "https://github.com/nvim-mini/mini.pick" },
    -- Manage lsps
    { src = "https://github.com/mason-org/mason.nvim" },           -- install
    { src = 'https://github.com/neovim/nvim-lspconfig' },          -- setup
    { src = 'https://github.com/mason-org/mason-lspconfig.nvim' }, -- setup
    -- Center tabs
    { src = "https://github.com/shortcuts/no-neck-pain.nvim" },
    -- Auto complete
    { src = "https://github.com/nvim-mini/mini.completion" },
})

require("oil").setup({
    default_file_explorer = true,
    columns = {
        "permissions",
        "size",
    },
    constrain_cursor = "name",
    watch_for_changes = true,
    keymaps = {
        ["g?"] = { "actions.show_help", mode = "n" },
        ["<CR>"] = "actions.select",
        ["<C-v>"] = { "actions.select", opts = { vertical = true } },
        ["<C-s>"] = { "actions.select", opts = { horizontal = true } },
        ["<C-t>"] = { "actions.select", opts = { tab = true } },
        ["<C-p>"] = "actions.preview",
        ["<C-c>"] = { "actions.close", mode = "n" },
        ["<C-l>"] = "actions.refresh",
        ["-"] = { "actions.parent", mode = "n" },
        ["_"] = { "actions.open_cwd", mode = "n" },
        ["`"] = { "actions.cd", mode = "n" },
        ["~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
        ["gs"] = { "actions.change_sort", mode = "n" },
        ["gx"] = "actions.open_external",
        ["g."] = { "actions.toggle_hidden", mode = "n" },
        ["g\\"] = { "actions.toggle_trash", mode = "n" },
    },
    view_options = {
        show_hidden = true,
    },
})

require("miniharp").setup({
    autoload = true,
    autosave = true,
    show_on_autoload = true,
})

require("mini.pick").setup()

require("mason").setup({
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
        },
    },
})

require("mason-lspconfig").setup()

require("mini.completion").setup()

-- Allows buffers to be centered, this helps with wide monitor support
--
-- The edges will be used as a notepad :)
require("no-neck-pain").setup({
    buffers = {
        right = { enabled = false, },
        scratchPad = {
            enabled = true, -- Autosave
            location = "~/notes/",
        },
        bo = {
            filetype = "md"
        },
    }
})

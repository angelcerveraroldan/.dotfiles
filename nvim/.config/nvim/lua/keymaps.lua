local s = { silent = true }

vim.g.mapleader = " " -- Make the super key be space

vim.keymap.set("n", "<space>", "<Nop>")
vim.keymap.set("n", "<C-d>", "<C-d>zz") -- Scroll down and center the cursor
vim.keymap.set("n", "<C-u>", "<C-u>zz") -- Scroll up and center the cursor
vim.keymap.set("n", "<Leader>w", "<cmd>w!<CR>", s) -- Save the current file
vim.keymap.set("n", "<Leader>q", "<cmd>q<CR>", s) -- Quit Neovim
vim.keymap.set("n", "<Leader>te", "<cmd>tabnew<CR>", s) -- Open a new tab

-- Window navigation
vim.keymap.set("n", "<Leader>wv", "<cmd>vsplit<CR>", s) -- Split the window vertically
vim.keymap.set("n", "<Leader>wh", "<cmd>split<CR>", s) -- Split the window horizontally
vim.keymap.set("n", "<Leader>wk", "<C-w>k") -- Move upwards one window
vim.keymap.set("n", "<Leader>wj", "<C-w>j") -- Move downwards one window
vim.keymap.set("n", "<Leader>wl", "<C-w>l") -- Move to the left window
vim.keymap.set("n", "<Leader>wh", "<C-w>h") -- Move to the right window

vim.keymap.set("n", "<Leader>fo", ":lua vim.lsp.buf.format()<CR>", s) -- Format the current buffer using LSP
vim.keymap.set("v", "<Leader>p", '"_dP') -- Paste without overwriting the default register
vim.keymap.set("x", "y", [["+y]], s) -- Yank to the system clipboard in visual mode

-- LSP bindings
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<Leader>ld", "<cmd>lua vim.lsp.buf.definition()<CR>", opts) -- Go to definition
vim.keymap.set("n", "<Leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>" )
vim.keymap.set("n", "<Leader>le", "<cmd>lua vim.diagnostic.open_float()<Cr>")

-------------------
-- PUGIN KEYMAPS --
-------------------

-- Oil keymaps
vim.keymap.set("n", "<leader>ft", "<cmd>Oil<CR>",
    { desc = "Open the file explorer" })

-- Miniharp keymaps
vim.keymap.set("n", "<Leader>mt", require("miniharp").toggle_file)
vim.keymap.set("n", "<Leader>ml", require("miniharp").show_list)
vim.keymap.set("n", "<Leader>mn", require("miniharp").next)
vim.keymap.set("n", "<Leader>mp", require("miniharp").prev)

-- Minipick keymaps
vim.keymap.set("n", "<Leader>sf", "<cmd>Pick files<CR>")
vim.keymap.set("n", "<Leader>sg", "<cmd>Pick grep_live<CR>")

-- No more neck pain keymaps
vim.keymap.set("n", "<leader>ob", "<cmd>NoNeckPain<CR>",
    { desc = "Open side buffer" })

-- Miniautocomplete keymaps


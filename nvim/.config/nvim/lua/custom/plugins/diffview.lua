-- View Git Diffs
-- https://github.com/sindrets/diffview.nvim

return {
  'sindrets/diffview.nvim',
  config = function()
    require('diffview').setup {}

    vim.keymap.set('n', '<leader>hd', '<cmd>DiffviewOpen<CR>', { desc = 'Git [H]unk [D]iff View' })
    vim.keymap.set('n', '<leader>hq', '<cmd>DiffviewClose<CR>', { desc = 'Git [H]unk Diff [Q]uit' })

    -- Show :
    local wk = require 'which-key'
    wk.register { '<leader>g', group = 'git' }
  end,
}

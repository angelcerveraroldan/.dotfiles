-- Nicer and prettier UI windows

return {
  'folke/noice.nvim',
  event = 'VeryLazy',
  dependencies = {
    'MunifTanjim/nui.nvim',
    'rcarriga/nvim-notify',
  },
  opts = {
    lsp = {
      progress = {
        enabled = true,
      },
      override = {
        ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
        ['vim.lsp.util.stylize_markdown'] = true,
        ['cmp.entry.get_documentation'] = true,
      },
      hover = {
        enabled = true,
        silent = false,
      },
      signature = {
        enabled = true,
      },
    },
    presets = {
      bottom_search = true, -- use a classic bottom command-line for search
      command_palette = true, -- position the command line and popup together
      long_message_to_split = true, -- long messages go to split
      inc_rename = false, -- don't override :IncRename
      lsp_doc_border = true, -- add border to LSP hover/signature help
    },
  },
}

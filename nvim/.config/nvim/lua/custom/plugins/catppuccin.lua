return {
  -- Catppuccin theme
  -- https://github.com/catppuccin/nvim
  'catppuccin/nvim',
  name = 'catppuccin',
  priority = 1000, -- make sure it loads before everything else

  config = function()
    require('catppuccin').setup {
      -- latte, frappe, macchiato, mocha
      flavour = 'auto',
      background = {
        light = 'latte',
        dark = 'mocha',
      },
      transparent_background = false,

      float = {
        transparent = false,
        solid = false,
      },

      show_end_of_buffer = false,
      term_colors = false,

      dim_inactive = {
        enabled = false,
        shade = 'dark',
        percentage = 0.15,
      },

      no_italic = false,
      no_bold = false,
      no_underline = false,

      styles = {
        comments = {},
        conditionals = { 'italic' },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
      },

      lsp_styles = {
        virtual_text = {
          errors = { 'italic' },
          hints = { 'italic' },
          warnings = { 'italic' },
          information = { 'italic' },
          ok = { 'italic' },
        },
        underlines = {
          errors = { 'underline' },
          hints = { 'underline' },
          warnings = { 'underline' },
          information = { 'underline' },
          ok = { 'underline' },
        },
        inlay_hints = {
          background = true,
        },
      },

      color_overrides = {},
      custom_highlights = {},

      default_integrations = true,
      auto_integrations = false,

      integrations = {
        cmp = true,
        gitsigns = true,
        nvimtree = true,
        notify = false,
        mini = {
          enabled = true,
          indentscope_color = '',
        },
        -- add/toggle more integrations here if you use them
        telescope = true,
        treesitter = true,
        which_key = true,
      },
    }

    -- load the colorscheme
    vim.cmd.colorscheme 'catppuccin'
  end,
}

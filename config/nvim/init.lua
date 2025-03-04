-- init.lua
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Set leader keys before lazy setup
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Plugin specifications
require("lazy").setup({
  -- Clojure Development
  {
    "Olical/conjure",
    branch = "develop",
    ft = {"clojure", "fennel"},
    config = function()
      vim.g["conjure_log_direction"] = "horizontal"
      vim.g["conjure#client#clojure#nrepl#connection#auto_repl#enabled"] = false
    end
  },
  { "bakpakin/fennel.vim" },
  { "tpope/vim-salve" },
  { "tpope/vim-projectionist" },
  { "tpope/vim-dispatch" },
  { "jrdoane/vim-clojure-highlight" },
  { "guns/vim-clojure-static" },
  { "guns/vim-sexp" },
  { "tpope/vim-sexp-mappings-for-regular-people" },
  
  -- Git Integration
  { "tpope/vim-fugitive" },
  { "tpope/vim-rhubarb" },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup({
        signs = {
          add          = { text = '│' },
          change       = { text = '│' },
          delete       = { text = '_' },
          topdelete    = { text = '‾' },
          changedelete = { text = '~' },
          untracked    = { text = '┆' },
        },
        on_attach = function(bufnr)
          local gs = package.loaded.gitsigns
          
          local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
          end
          
          -- Navigation
          map('n', ']c', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.next_hunk() end)
            return '<Ignore>'
          end, {expr=true})
          
          map('n', '[c', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.prev_hunk() end)
            return '<Ignore>'
          end, {expr=true})
          
          -- Actions
          map('n', '<leader>hs', gs.stage_hunk)
          map('n', '<leader>hr', gs.reset_hunk)
          map('v', '<leader>hs', function() gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
          map('v', '<leader>hr', function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
          map('n', '<leader>hS', gs.stage_buffer)
          map('n', '<leader>hu', gs.undo_stage_hunk)
          map('n', '<leader>hR', gs.reset_buffer)
          map('n', '<leader>hp', gs.preview_hunk)
          map('n', '<leader>hb', function() gs.blame_line{full=true} end)
          map('n', '<leader>tb', gs.toggle_current_line_blame)
          map('n', '<leader>hd', gs.diffthis)
          map('n', '<leader>hD', function() gs.diffthis('~') end)
          map('n', '<leader>td', gs.toggle_deleted)
        end
      })
    end
  },
  
  -- Editing Enhancement
  { "tpope/vim-sensible" },
  { "tpope/vim-commentary" },
  { "tpope/vim-surround" },
  { "roman/golden-ratio" },
  { "ntpeters/vim-better-whitespace" },
  
  -- Telescope and Dependencies
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make'
      }
    },
    config = function()
      local telescope = require('telescope')
      local actions = require('telescope.actions')
      
      telescope.setup({
        defaults = {
          layout_strategy = 'vertical',
          layout_config = {
            height = 0.6,  -- Similar to your FZF 60% height
          },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
              ["<C-u>"] = false,
            },
          },
        },
      })
      
      -- Enable FZF native sorter
      telescope.load_extension('fzf')
    end
  },
  
  -- LSP Support
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      -- Completion Framework
      "hrsh7th/nvim-cmp",
      -- LSP completion source
      "hrsh7th/cmp-nvim-lsp",
      -- Useful completion sources
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      -- Snippets
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      -- Initialize Mason before setting up LSP
      require("mason").setup({
        ui = {
          border = "rounded",
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
          }
        }
      })
      
      require("mason-lspconfig").setup({
        ensure_installed = {
          "clojure_lsp",  -- For Clojure
        },
        automatic_installation = true
      })

      -- Get the LSP configuration module
      local lsp_config = require("core.lsp")
      
      -- Setup nvim-cmp
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      
      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-d>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          },
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = {
          { name = "nvim_lsp" },
          { name = "nvim_lua" },
          { name = "luasnip" },
          { name = "buffer" },
          { name = "path" },
        },
      })
      
      -- Update capabilities for LSP
      lsp_config.capabilities = require("cmp_nvim_lsp").default_capabilities()
      
      -- Configure LSP servers
      local lspconfig = require("lspconfig")
      
      -- Clojure LSP setup
      lspconfig.clojure_lsp.setup({
        on_attach = lsp_config.on_attach,
        capabilities = lsp_config.capabilities,
      })
    end
  },
  
  -- Linting (clj-kondo is now handled by clojure-lsp)
  
  -- Status Line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup({
        options = {
          icons_enabled = true,
          theme = 'gruvbox',
          component_separators = { left = '', right = ''},
          section_separators = { left = '', right = ''},
          disabled_filetypes = {
            statusline = {},
            winbar = {},
          },
          ignore_focus = {},
          always_divide_middle = true,
          globalstatus = false,
        },
        sections = {
          lualine_a = {'mode'},
          lualine_b = {
            'branch',
            {
              'diff',
              colored = true,
              symbols = {added = ' ', modified = ' ', removed = ' '}
            },
            'diagnostics'
          },
          lualine_c = {
            {
              'filename',
              path = 1,  -- Show relative path
              symbols = {
                modified = '[+]',
                readonly = '[RO]',
                unnamed = '[No Name]',
              }
            }
          },
          lualine_x = {'encoding', 'fileformat', 'filetype'},
          lualine_y = {'progress'},
          lualine_z = {'location'}
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {'location'},
          lualine_y = {},
          lualine_z = {}
        },
      })
    end
  },
  -- avante ai coding --
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    lazy = false,
    version = false, -- Set this to "*" to always pull the latest release version, or set it to false to update to the latest code changes.
    -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
    build = "make",
    opts = {},
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "echasnovski/mini.pick", -- for file_selector provider mini.pick
      "nvim-telescope/telescope.nvim", -- for file_selector provider telescope
      "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
      "ibhagwan/fzf-lua", -- for file_selector provider fzf
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      -- "zbirenbaum/copilot.lua", -- for providers='copilot'
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
      {
        -- Make sure to set this up properly if you have lazy=true
        'MeanderingProgrammer/render-markdown.nvim',
        opts = {
          file_types = { "markdown", "Avante" },
        },
        ft = { "markdown", "Avante" },
      },
    },
  },
  -- Color Schemes
  { "rafi/awesome-vim-colorschemes" },
  { "rakr/vim-two-firewatch" },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate"
  },

  -- HCL Support
  { "jvirtanen/vim-hcl" },
})

-- Load core configuration
require("core.options")

-- Set colorscheme
vim.cmd([[colorscheme gruvbox]])

-- Disable netrw history
vim.g.netrw_dirhistmax = 0

-- Show trailing whitespace as error
vim.cmd([[match ErrorMsg '\s\+$']])

-- Autocommands
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"lisp", "clojure", "racket"},
  callback = function()
    vim.bo.commentstring = ";; %s"
  end
})

-- Key Mappings
local function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- General Mappings
map("i", "kj", "<ESC>")
map("t", "kj", "<C-\\><C-n>")
map("n", "<Leader>w", ":w<CR>")
map("n", "<Leader>r", ":wa<CR>:ConjureEval (integrant.repl/reset)<CR>")
map("n", "<D-s>", ":w<CR>")
map("i", "<D-s>", "<Esc>:w<CR>a")
-- map("n", "<Leader>y", '"*y')
-- map("n", "<Leader>f", "zfaF")
map("n", "<C-u>", "10k")
map("n", "<C-d>", "10j")
map("n", "gev", ":e $MYVIMRC<CR>")
map("n", "gsv", ":so $MYVIMRC<CR>")

-- Telescope Mappings (replacing FZF)
local telescope_builtin = require('telescope.builtin')
map("n", "<Leader><Leader>", function() telescope_builtin.commands() end)
-- map("n", "<Leader>a", function() telescope_builtin.live_grep() end)
-- map("n", "<Leader>A", function() telescope_builtin.grep_string() end)
-- map("n", "<Leader>p", function() telescope_builtin.git_files() end)
-- map("n", "<Leader>b", function() telescope_builtin.buffers() end)
-- map("n", "<Leader>m", function() telescope_builtin.marks() end)
-- map("n", "<Leader>l", function() telescope_builtin.current_buffer_fuzzy_find() end)

-- Fugitive Mappings
-- map("n", "<Leader>gb", ":Git blame<CR>")
-- map("n", "<Leader>gd", ":Git diff<CR>")
-- map("n", "<Leader>gc", ":Git commit<CR>")
-- map("n", "<Leader>gs", ":Git status<CR>")
-- map("n", "<Leader>gS", ":Git status<CR><C-w>T")

-- Visual mode mappings
-- map("x", "<Leader>t", 'sai{{t "<CR>"}}<CR>')

-- Commands
vim.api.nvim_create_user_command("MigrationTimeStamp", "r !date -u '+\\%Y-\\%m-\\%d--\\%H-\\%M'", {})
vim.api.nvim_create_user_command("PasteAsHiccup", "r !pbpaste | html2hiccup", {})

-- Clojure specific settings
vim.g.clojure_align_subforms = 1

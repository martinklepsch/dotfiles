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
  
  -- Modern LSP Setup
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "folke/neodev.nvim", -- for better Lua development
    },
  },
  
  -- Modern Completion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
  },
  
  -- Git Integration
  {
    "lewis6991/gitsigns.nvim", -- Modern replacement for gitgutter
    config = true,
  },
  { "tpope/vim-fugitive" },
  { "tpope/vim-rhubarb" },
  
  -- Modern Statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require('lualine').setup({
        options = {
          theme = 'gruvbox',
          component_separators = '|',
          section_separators = '',
        },
      })
    end,
  },
  
  -- Better syntax highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          "lua", "vim", "vimdoc", "javascript", 
          "typescript", "clojure", "query"
        },
        highlight = { enable = true },
        indent = { enable = true },
      })
    end,
  },
  
  -- Editing Enhancement
  { "tpope/vim-commentary" },
  {
    "kylechui/nvim-surround", -- Modern surround
    config = true,
  },
  { "roman/golden-ratio" },
  { "ntpeters/vim-better-whitespace" },
  
  -- Telescope setup
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
          layout_config = { height = 0.6 },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
              ["<C-u>"] = false,
            },
          },
        },
      })
      telescope.load_extension('fzf')
    end
  },
  
  -- Modern UI Enhancements
  {
    "folke/which-key.nvim",
    config = true,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = true,
  },
  
  -- Modern Colorscheme
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
  },
})

-- LSP Configuration
require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = { "clojure_lsp", "lua_ls" },
})

local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- Setup completion
local cmp = require('cmp')
cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'buffer' },
    { name = 'path' },
  })
})

-- Configure LSP servers
lspconfig.clojure_lsp.setup({
  capabilities = capabilities,
})

lspconfig.lua_ls.setup({
  capabilities = capabilities,
  settings = {
    Lua = {
      diagnostics = {
        globals = { 'vim' }
      }
    }
  }
})

-- LSP Keybindings
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
  end,
})

-- Base Configuration
local opt = vim.opt

opt.swapfile = false
opt.foldmethod = "expr"
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldlevelstart = 20
opt.foldenable = false
opt.background = "dark"
opt.hlsearch = true
opt.ignorecase = true
opt.smartcase = true  -- Added smart case search
opt.cursorline = true
opt.confirm = true
opt.hidden = true
opt.termguicolors = true
opt.fixendofline = false
opt.shiftwidth = 2
opt.expandtab = true
opt.number = true     -- Added line numbers
opt.relativenumber = true  -- Added relative line numbers
opt.scrolloff = 8     -- Keep 8 lines above/below cursor
opt.sidescrolloff = 8 -- Keep 8 columns left/right of cursor
opt.updatetime = 300  -- Faster completion
opt.timeout = true
opt.timeoutlen = 300  -- Faster which-key

-- Set colorscheme
vim.cmd([[colorscheme catppuccin]])

-- Show trailing whitespace as error
vim.cmd([[match ErrorMsg '\s\+$']])

-- Autocommands
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"lisp", "clojure", "racket"},
  callback = function()
    vim.bo.commentstring = ";; %s"
  end
})

-- Key Mappings (keeping your existing ones and adding new ones)
local function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- Your existing mappings
map("i", "kj", "<ESC>")
map("t", "kj", "<C-\\><C-n>")
map("n", "<Leader>w", ":w<CR>")
map("n", "<Leader>r", ":wa<CR>:ConjureEval (integrant.repl/reset)<CR>")
map("n", "<D-s>", ":w<CR>")
map("i", "<D-s>", "<Esc>:w<CR>a")
map("n", "<Leader>y", '"*y')
map("n", "<Leader>f", "zfaF")
map("n", "<C-u>", "10k")
map("n", "<C-d>", "10j")
map("n", "gev", ":e $MYVIMRC<CR>")
map("n", "gsv", ":so $MYVIMRC<CR>")

-- Telescope Mappings
local telescope_builtin = require('telescope.builtin')
map("n", "<Leader><Leader>", function() telescope_builtin.commands() end)
map("n", "<Leader>a", function() telescope_builtin.live_grep() end)
map("n", "<Leader>A", function() telescope_builtin.grep_string() end)
map("n", "<Leader>p", function() telescope_builtin.git_files() end)
map("n", "<Leader>b", function() telescope_builtin.buffers() end)
map("n", "<Leader>m", function() telescope_builtin.marks() end)
map("n", "<Leader>l", function() telescope_builtin.current_buffer_fuzzy_find() end)

-- Fugitive Mappings
map("n", "<Leader>gb", ":Git blame<CR>")
map("n", "<Leader>gd", ":Git diff<CR>")
map("n", "<Leader>gc", ":Git commit<CR>")
map("n", "<Leader>gs", ":Git status<CR>")
map("n", "<Leader>gS", ":Git status<CR><C-w>T")

-- Visual mode mappings
map("x", "<Leader>t", 'sai{{t "<CR>"}}<CR>')

-- Commands
vim.api.nvim_create_user_command("MigrationTimeStamp", "r !date -u '+\\%Y-\\%m-\\%d--\\%H-\\%M'", {})
vim.api.nvim_create_user_command("PasteAsHiccup", "r !pbpaste | html2hiccup", {})

-- Clojure specific settings
vim.g.clojure_align_subforms = 1
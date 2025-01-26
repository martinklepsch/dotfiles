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
  { "airblade/vim-gitgutter" },
  
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
  
  -- Completion and LSP
  {
    "neoclide/coc.nvim",
    branch = "release"
  },
  
  -- Linting
  {
    "dense-analysis/ale",
    config = function()
      vim.g.ale_linters = {
        clojure = { "clj-kondo" },
        javascript = { "eslint" }
      }
    end
  },
  
  -- Status Line
  { "itchyny/lightline.vim" },
  
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

-- Base Configuration
local opt = vim.opt

opt.swapfile = false
opt.foldmethod = "syntax"
opt.foldlevelstart = 20
opt.foldenable = false
opt.background = "dark"
opt.hlsearch = true
opt.ignorecase = true
opt.cursorline = true
opt.confirm = true
opt.hidden = true
opt.termguicolors = true
opt.fixendofline = false
opt.shiftwidth = 2
opt.expandtab = true

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
map("n", "<Leader>y", '"*y')
map("n", "<Leader>f", "zfaF")
map("n", "<C-u>", "10k")
map("n", "<C-d>", "10j")
map("n", "gev", ":e $MYVIMRC<CR>")
map("n", "gsv", ":so $MYVIMRC<CR>")

-- Telescope Mappings (replacing FZF)
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
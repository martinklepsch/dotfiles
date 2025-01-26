-- init.lua

-- Set leader keys before anything else
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Bootstrap lazy.nvim
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

-- Load configurations
require("options")      -- Core vim options
require("keymaps")      -- Key mappings
require("autocmds")     -- Auto commands
require("lazy").setup(require("plugins"))

-- Set colorscheme
vim.cmd([[colorscheme gruvbox]])

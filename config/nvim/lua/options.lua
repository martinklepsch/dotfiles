-- Core Vim Options
local opt = vim.opt

-- File handling
opt.swapfile = false
opt.hidden = true
opt.fixendofline = false

-- Editing
opt.shiftwidth = 2
opt.expandtab = true
opt.cursorline = true

-- Search
opt.hlsearch = true
opt.ignorecase = true

-- Folding
opt.foldmethod = "syntax"
opt.foldlevelstart = 20
opt.foldenable = false

-- Visual
opt.background = "dark"
opt.termguicolors = true
opt.confirm = true

-- Disable netrw history
vim.g.netrw_dirhistmax = 0

-- Show trailing whitespace as error
vim.cmd([[match ErrorMsg '\s\+$']])

-- Clojure specific settings
vim.g.clojure_align_subforms = 1

-- Return the module (optional but good practice)
return {}

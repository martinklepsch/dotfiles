-- Key Mapping Configuration
local M = {}

-- Helper function for mapping keys
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

return M

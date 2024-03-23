local function cljstyle_fix()
  local cwd = vim.fn.getcwd()
  local winsave = vim.fn.winsaveview()
  vim.cmd('cd ' .. vim.fn.expand('%:p:h'))

  vim.cmd('%!cljstyle pipe')

  vim.cmd('cd ' .. cwd)
  vim.fn.winrestview(winsave)
end

vim.filetype.add({
  filename = {
    ['.cljstyle'] = 'clojure'
  }
})

vim.keymap.set('n', '<leader>cs', cljstyle_fix, { noremap = true })

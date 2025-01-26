-- Autocommands Configuration

-- Clojure file type settings
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"lisp", "clojure", "racket"},
  callback = function()
    vim.bo.commentstring = ";; %s"
  end
})

-- Return empty module (good practice)
return {}

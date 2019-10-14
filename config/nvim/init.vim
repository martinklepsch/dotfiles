call plug#begin('~/.vim/plugged')

" Plug 'Olical/conjure', { 'tag': 'v2.0.0', 'do': 'bin/compile'  }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'roman/golden-ratio'
Plug 'jrdoane/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'kien/rainbow_parentheses.vim'
Plug 'rakr/vim-two-firewatch'

call plug#end()

" https://github.com/guns/vim-clojure-highlight#rainbow-parentheses-and-other-syntax-extensions
autocmd Syntax clojure EnableSyntaxExtension
autocmd VimEnter *       RainbowParenthesesToggle
autocmd Syntax   clojure RainbowParenthesesLoadRound
autocmd Syntax   clojure RainbowParenthesesLoadSquare
autocmd Syntax   clojure RainbowParenthesesLoadBraces

syntax on
filetype plugin indent on

let mapleader = " "
nnoremap <Leader>a :Rg<CR>
nnoremap <Leader>p :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>w :w<CR>

" https://superuser.com/a/1120318
inoremap kj <ESC>
nnoremap gev :e $MYVIMRC<CR>
nnoremap gsv :so $MYVIMRC<CR>

" https://stackoverflow.com/questions/9850360/what-is-netrwhist
let g:netrw_dirhistmax = 0

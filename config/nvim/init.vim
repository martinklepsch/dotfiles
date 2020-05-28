" Plugin List {{{
call plug#begin('~/.vim/plugged')

Plug 'Olical/conjure', { 'branch': 'develop' }
Plug 'bakpakin/fennel.vim'
" Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
" Plug 'clojure-vim/acid.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'liquidz/vim-iced', {'for': 'clojure'}

Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-surround'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'voldikss/vim-floaterm'
Plug 'roman/golden-ratio'
Plug 'jrdoane/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'rakr/vim-two-firewatch'
Plug 'luochen1990/rainbow'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'ntpeters/vim-better-whitespace'
Plug 'dense-analysis/ale'
Plug 'itchyny/lightline.vim'

Plug 'rafi/awesome-vim-colorschemes'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-colorscheme-switcher'

call plug#end() "}}}

" Base Config {{{
colorscheme gruvbox
set noswapfile
set foldmethod=syntax
set foldlevelstart=20
set bg=dark
set hlsearch
set ignorecase
set cursorline
set confirm
set hidden
set termguicolors
let g:netrw_dirhistmax = 0 " https://stackoverflow.com/questions/9850360/what-is-netrwhist
match ErrorMsg '\s\+$'"}}}

" function! g:FoldLispTopForm(lnum)
"   let curr = getline(a:lnum)
"   let next = getline(a:lnum+1)
"   if next[0] == '('
"     return 0
"   elseif curr[0] == '('
"     return 1
"   endif
"   return '='
" endfunction

" autocmd FileType clojure,lisp,scheme setlocal foldmethod=expr foldexpr=g:FoldLispTopForm(v:lnum)

" Plugin Config {{{
"
let g:rainbow_active = 1
" https://github.com/borkdude/clj-kondo/blob/master/doc/editor-integration.md#vim--neovim
let g:ale_linters = {'clojure': ['clj-kondo']}

let g:conjure_log_direction = "horizontal"
let g:floaterm_autoclose = 1

" }}}

" Mappings {{{
let mapleader = " "
let maplocalleader = "\\"
nnoremap <localleader>f zA

" General Mappings {{{
inoremap kj <ESC>
tnoremap kj <C-\><C-n>
nnoremap <Leader>w :w<CR>
noremap <Leader>y "*y
nmap <Leader>f zfaF
nnoremap <C-u> 10k
nnoremap <C-d> 10j
nnoremap gev :e $MYVIMRC<CR>
nnoremap gsv :so $MYVIMRC<CR>
"}}}

" FZF.vim mappings{{{
nnoremap <Leader><Leader> :Commands<CR>
nnoremap <Leader>a :Rg<CR>
nnoremap <Leader>p :GFiles . ':!:_site/*'<CR>
nnoremap <Leader>b :Buffers<CR>
"}}}

" Fugitive mappings{{{
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gS :Gstatus<CR><C-w>T
"}}}

" Lazygit mappings{{{
nmap <Leader>g :FloatermNew lazygit<CR>
"}}}

" indent forms after slurping
nnoremap >) >)==

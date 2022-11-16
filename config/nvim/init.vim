" https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

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
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-rhubarb'
Plug 'machakann/vim-sandwich'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'roman/golden-ratio'
Plug 'jrdoane/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'mustache/vim-mustache-handlebars'
Plug 'luochen1990/rainbow'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'ntpeters/vim-better-whitespace'
Plug 'dense-analysis/ale'
Plug 'itchyny/lightline.vim'

Plug 'rafi/awesome-vim-colorschemes'
Plug 'xolox/vim-misc'
Plug 'rakr/vim-two-firewatch'
Plug 'xolox/vim-colorscheme-switcher'

Plug 'hashivim/vim-terraform'
Plug 'cespare/vim-toml'

Plug 'editorconfig/editorconfig-vim'

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
set nofixendofline
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
augroup commentary_config
  autocmd!
  autocmd FileType lisp,clojure,racket setlocal commentstring=;;\ %s
augroup END

" Plugin Config {{{
"
" let g:rainbow_active = 1
" https://github.com/borkdude/clj-kondo/blob/master/doc/editor-integration.md#vim--neovim
let g:ale_linters = {'clojure': ['clj-kondo']}

let g:conjure_log_direction = "horizontal"
let g:conjure#client#clojure#nrepl#connection#auto_repl#enabled = v:false
let g:floaterm_autoclose = 1

let g:fzf_layout = { 'down': '60%' }

" https://github.com/bbatsov/clojure-style-guide#arguments-indentation
let g:clojure_align_subforms = 1

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
nnoremap <Leader>A :Rg <C-R><C-W><CR>
nnoremap <Leader>p :GFiles . ':!:_site/*'<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>l :Lines<CR>
"}}}

" Fugitive mappings{{{
nmap <Leader>gb :Git blame<CR>
nmap <Leader>gd :Git diff<CR>
nmap <Leader>gc :Git commit<CR>
nmap <Leader>gs :Git status<CR>
nmap <Leader>gS :Git status<CR><C-w>T
"}}}

" Lazygit mappings{{{
" nmap <Leader>g :term lg<CR>i
"}}}
"
xmap <Leader>t sai{{t "<CR>"}}<CR>

" indent forms after slurping
nnoremap >) >)==
nnoremap <leader>cs :call cljstyle#fix()<cr>

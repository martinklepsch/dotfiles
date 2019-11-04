" Plugin List {{{
call plug#begin('~/.vim/plugged')

" Plug 'Olical/conjure', { 'tag': 'v2.0.0', 'do': 'bin/compile'  }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'roman/golden-ratio'
Plug 'jrdoane/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'kien/rainbow_parentheses.vim'
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
colorscheme space-vim-dark
set noswapfile
set foldmethod=manual
set bg=dark
set hlsearch
set ignorecase
set cursorline
set confirm
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
" }}}

" Mappings {{{
let mapleader = " "

" General Mappings {{{
inoremap kj <ESC>
tnoremap kj <C-\><C-n>
nnoremap <Leader>w :w<CR>
noremap <Leader>y "*y
nnoremap <C-u> 10k
nnoremap <C-d> 10j
nnoremap gev :e $MYVIMRC<CR>
nnoremap gsv :so $MYVIMRC<CR>
"}}}

" FZF.vim mappings{{{
nnoremap <Leader><Leader> :Commands<CR>
nnoremap <Leader>a :Rg<CR>
nnoremap <Leader>p :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
"}}}

" Fugitive mappings{{{
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gS :Gstatus<CR><C-w>T
"}}}

" GitGutter Mappings {{{
" (from https://github.com/SevereOverfl0w/.files/blob/304610fca3437f4d22c672cb6d5d6e81f004664c/nvim/dein-plugin/git.vim#L16-L26)
 " Mapping for jumping between hunks
let g:gitgutter_map_keys = 0
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
" Stage the hunk under the cursor
nmap <Leader>ghs <Plug>(GitGutterStageHunk)
" Show the diff of the hunk at cursor.  I'm not convinced this is actually
" useful yet, but time will tell.
nmap <Leader>ghp <Plug>(GitGutterPreviewHunk)
" Discard the hunk under the cursor.  Useful for getting rid of println code.
nmap <Leader>ghu <Plug>(GitGutterUndoHunk)"}}}}}}

" autocmd! gitgutter CursorHold,CursorHoldI
" autocmd BufWritePost * GitGutter

" indent forms after slurping
nnoremap >) >)==

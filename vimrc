" Plug {{{1
let g:plug_shallow = 0
let g:plug_window  = 'enew'
let g:plug_pwindow = 'vertical rightbelow new'

call plug#begin('~/.vim/plugged')
  " General
  Plug 'gmarik/vundle'

  " Ack
  Plug 'mileszs/ack.vim'

  let g:ackprg = 'ag --nogroup --nocolor --column'

  Plug 'vim-scripts/IndexedSearch'

  " Git
  Plug 'tpope/vim-fugitive'
  Plug 'gregsexton/gitv'
  nmap <leader>g :Gstatus<cr>

  Plug 'tpope/vim-unimpaired'
  Plug 'godlygeek/tabular'
  Plug 'Raimondi/delimitMate'
  Plug 'tpope/vim-surround'

  " Snipmate
  Plug 'MarcWeber/vim-addon-mw-utils'
  Plug 'tomtom/tlib_vim'
  Plug 'garbas/vim-snipmate'

  Plug 'w0rp/ale'

  let g:ale_linters = {'javascript': ['eslint']}
  let g:ale_javascript_eslint_executable = './node_modules/.bin/eslint'
  let g:ale_sign_column_always = 1
  let g:ale_sign_error = '>'
  let g:ale_sign_warning = '-'
  let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']

  " Javascript
  Plug 'pangloss/vim-javascript'
  Plug 'mxw/vim-jsx'

  " Dont require jsx file extension for enabling this plugin
  let g:jsx_ext_required = 0

  " Plug 'JavaScript-Indent'
  Plug 'elzr/vim-json'
  Plug 'vim-scripts/ParseJSON'

  " Gist
  Plug 'mattn/webapi-vim'
  Plug 'mattn/gist-vim'

  let g:gist_detect_filetype = 1
  let g:gist_show_privates = 1
  let g:gist_get_multiplefile = 1
  let g:gist_clip_command = 'pbcopy'
  let g:gist_browser_command = '/usr/bin/open -a "/Applications/Google Chrome.app" "%URL%"'

  " Vimux
  Plug 'benmills/vimux'

  " Syntax Attribute Checker
  Plug 'vim-scripts/SyntaxAttr.vim'

  let g:airline_left_sep=''
  let g:airline_right_sep=''

  " Markdown
  Plug 'itspriddle/vim-marked', { 'for': 'markdown' }

  " Bash
  Plug 'WolfgangMehner/bash-support'

  " CtrlP
  Plug 'kien/ctrlp.vim'

  let g:ctrlp_use_caching = 1
  let g:ctrlp_clear_cache_on_exit = 1
  let g:ctrlp_working_path_mode = ''

  nmap <leader>b :CtrlPBuffer<CR>

  " Initialize ctrlp delete buffer from autoload (there is no bundle plugin yet)
  " Use C-@ to delete buffer
  call ctrlp_bdelete#init()

  if executable('ag')
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  endif

  " NERDTree
  Plug 'scrooloose/nerdtree'

  let g:NERDTreeChDirMode = 2
  let g:NERDTreeMinimalUI = 1
  let g:NERDTreeIgnore=['node_modules', 'build', 'coverage']

  " Prevent deleting nerdtree buffer
  autocmd FileType nerdtree cnoreabbrev <buffer> bd :call g:WorkaroundNERDTreeToggle()<CR>

  function! g:WorkaroundNERDTreeToggle()
    try | :NERDTreeToggle | catch | :enew | endtry
    echo ''
  endfunction

  nnoremap [b :BB<CR>
  nnoremap ]b :BF<CR>

  Plug 'ElmCast/elm-vim'
  Plug 'jnwhiteh/vim-golang'
  Plug 'othree/html5.vim'
  Plug 'groenewege/vim-less'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'vim-scripts/DrawIt'
  Plug 'sjl/vitality.vim'
  Plug 'kovisoft/slimv'
  Plug 'tpope/vim-fireplace'
  Plug 'guns/vim-clojure-static'
  Plug 'kien/rainbow_parentheses.vim'
  Plug 'guns/vim-clojure-highlight'
  Plug 'jparise/vim-graphql'
  Plug 'LnL7/vim-nix'
  Plug 'mhinz/vim-sayonara'

  " Testing phase.

  " Styled Components

  Plug 'styled-components/vim-styled-components'

  " TypeScript

  Plug 'leafgarland/typescript-vim'

  " ReasonML
  Plug 'reasonml-editor/vim-reason'

  " Prettier
  Plug 'prettier/vim-prettier', { 'do': 'npm install' }

  let g:prettier#exec_cmd_async = 1
  let g:prettier#autoformat = 1
  let g:prettier#quickfix_enabled = 0
  autocmd BufWritePre *.js,*.md PrettierAsync

  " Org Mode
  Plug 'tpope/vim-repeat'
  Plug 'vim-scripts/utl.vim'
  Plug 'vim-scripts/SyntaxRange'
  Plug 'tpope/vim-speeddating'
  " Silence vim-orgmode Speeddating missing
  command! -nargs=* -range SpeedDatingFormat
  Plug 'jceb/vim-orgmode'

  " More focus
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'

  " Better Status Line
  Plug 'itchyny/lightline.vim'

  function! s:goyo_enter()
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
    set noshowmode
    set noshowcmd
    set scrolloff=999
    Limelight
  endfunction

  function! s:goyo_leave()
    silent !tmux set status on
    silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
    set showmode
    set showcmd
    set scrolloff=5
    Limelight!
  endfunction

  autocmd! User GoyoEnter nested call <SID>goyo_enter()
  autocmd! User GoyoLeave nested call <SID>goyo_leave()
call plug#end()
" }}}1

set nocompatible               " be iMproved
filetype off                   " required!

" ale runtime path
let &runtimepath.=',~/.vim/bundle/ale'

let mapleader = " "

" Global Stuff
"-----------------------------------------------------------------------------

" Set filetype stuff to on
filetype plugin indent on
set ttyfast " Improves redrawing

set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
set smartindent

set clipboard^=unnamed,unnamedplus

" Necessary to show unicode glyphs
set encoding=utf-8

" Explicitly tell vim that the terminal supports 256 colors
set t_Co=256

" set the search scan to wrap lines
set wrapscan

"vset line wrapping off
set nowrap

" set the search scan so that it ignores case when the search is all lower
" case but recognizes uppercase if it's specified
set ignorecase
set smartcase

" set the forward slash to be the slash of note.  Backslashes suck
set shellslash
set shell=bash

" Make command line one lines high
set ch=1

" set visual bell -- i hate that damned beeping
set novb

" set line numbers
set nu

" Allow backspacing over indent, eol, and the start of an insert
set backspace=2

" Make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the "must save first" error doesn't come up)
set hidden

" Set the status line the way i like it
set stl=%f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

" tell VIM to always put a status line in, even if there is only one window
set laststatus=2

"" Don't update the display while executing macros
set lazyredraw

" Show the current command in the lower right corner
set showcmd

" Show the current mode
set showmode

" Switch on syntax highlighting.
syntax on

" Disable syntax for some files
autocmd! bufreadpost *.min.js set syntax=off

" Hide the mouse pointer while typing
set mousehide
set mouse=a

" Set mouse click are to be more compatible and wider on unsupported screens

" Set up the gui cursor to look nice
set guicursor=n-v-c:block-Cursor-blinkon0
set guicursor+=ve:ver35-Cursor
set guicursor+=o:hor50-Cursor
set guicursor+=i-ci:ver25-Cursor
set guicursor+=r-cr:hor20-Cursor
set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

" set the gui options the way I like
set guioptions=ac

" This is the timeout used while waiting for user input on a multi-keyed macro
" or while just sitting and waiting for another key to be pressed measured
" in milliseconds.
"
" i.e. for the ",d" command, there is a "timeoutlen" wait period between the
"	   "," key and the "d" key.  If the "d" key isn't pressed before the
"	   timeout expires, one of two things happens: The "," command is executed
"	   if there is one (which there isn't) or the command aborts.
set timeoutlen=500

" Keep some stuff in the history
set history=100

" These commands open folds
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo

set nofoldenable

" When the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8

" Allow the cursor to go in to "invalid" places
set virtualedit=all

" These things start comment lines
set comments=sl:/*,mb:\ *,ex:\ */,O://,b:#,:%,:XCOMM,n:>,fb:-

" Make the command-line completion better
set wildmenu

" Set where vim should look for aut completion
set complete=.,w,b,u,t,i,kspell

" When completing by tag, show the whole tag, not just the function name
set showfulltag

" No wrapping
set textwidth=0
set wrapmargin=0
set formatoptions-=t

" get rid of the silly characters in window separators
set fillchars=""

" Add ignorance of whitespace to diff
set diffopt+=iwhite

" Enable search highlighting
set hlsearch

" Incrementally match the search
set incsearch

" Set the tags files to be the following
" set tags+=vendor.tags

" Change startup messsage
set shortmess+=I

set background=dark

" Set color scheme to dejanr
let g:base16colorspace=256
colorscheme base16-default

" disable swapping
set nobackup
set noswapfile

" Auto read file when a file has been changed outside of vim
set autoread

" Toggle paste mode
nmap <silent> <leader>i :set invpaste<CR>:set paste?<CR>

" cd to the directory containing the file in the buffer
nmap <silent> <leader>cd :lcd %:h<CR>

" Turn off that stupid highlight search
nmap <silent> <leader>n :set invhls<CR>:set hls?<CR>

" Show all available VIM servers
nmap <silent> <leader>ss :echo serverlist()<CR>

" set text wrapping toggles
nmap <silent> <leader>w :set invwrap<CR>:set wrap?<CR>

" Edit the vimrc file
nmap <leader>ev :e $MYVIMRC<CR>
nmap <leader>sv :so $MYVIMRC<CR>

" Make horizontal scrolling easier
nmap <silent> <C-h> 20zl
nmap <silent> <C-l> 20zh

nmap <silent> <leader>q :Sayonara<CR>

" Syntax coloring lines that are too long just slows down the world
set synmaxcol=2048

" I don't like it when the matching parens are automatically highlighted
" let loaded_matchparen = 1

" Rebuild ctaglist
nmap <leader>cx :!ctags<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Omni complete functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

let g:snippets_dir = '~/.vim/snippets'

" Minimum window height = 0
set wmh=0

" Ignore this files when search files
set wildignore+=.git/*,*/.git/*,*.DS_Store,*/node_modules/*,*/dist/*
set wildignore+=*/cache/*,*/.sass-cache/*
set wildignore+=*/coverage/*,*/public/*,*-min.js,*-build.js

" When more then one result, match the first completion
set wildmode="list:full"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => PHP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Strip trailing whitespace
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
autocmd BufWritePre *.* :call <SID>StripTrailingWhitespaces()

" JSON
augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set softtabstop=2 tabstop=2
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=manual
augroup END

" Bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => MISC
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>rm :%s/<C-V><cr>//ge<cr>'tzt'm

" Quickly ope a buffer for scripbble
map <leader>oq :e ~/buffer<cr>
au BufRead,BufNewFile ~/buffer iab <buffer> xh1 ===========================================
au BufNewFile,BufRead *.rs set filetype=rust

" CommandT
let g:CommandTMaxHeight = 20
let g:CommandTMinHeight = 20

" Remap arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Use custom characters for tabstops and EOLs
set listchars=tab:▸\ ,eol:●

" Invisible character(tabstops, EOLs) custom color
highlight NonText guifg=#124956

" Toggle list, showing or hiding special chars
nmap <leader>l :setlocal list!<cr>

" Dont show special characters by default
set nolist

" Start daily report
nmap <silent> <leader>gn :enew<CR>:set ft=html<CR>idaily<tab>

" Vimux
let g:VimuxOrientation = "h"
let g:VimuxUseNearestPane = 1

nmap t :VimuxRunLastCommand<CR>
nmap <leader>s :w<CR>

set ttymouse=xterm2

" dont map stupid s
nnoremap s <nop>

set guifont=PragmataPro:h13
let g:Powerline_symbols='fancy'

" Shortcut for file explorer
nmap <leader>f :NERDTreeFocus<CR>

" netrw settings
let g:netrw_banner = 0
let g:netrw_browse_split = 0

nmap <leader>b :CtrlPBuffer<CR>

" Set dictionary (Its used with C-X C-K to autocomplete words)
set dictionary=/usr/share/dict/words

" Using gf to jump to node js file
set suffixesadd+=.js
set path+=$PWD/node_modules

nnoremap <leader>4 :e! %<cr>

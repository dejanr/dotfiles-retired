syntax on
syntax enable

set hlsearch " Enable search highlighting
set number " Show line numbers

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" no backups and swapping
set nobackup
set noswapfile

set ruler		" show the cursor position all the time
set cursorline
set showcmd		" display incomplete commands

set guifont=PragmataPro:h13

if has('mouse')
  set mouse=a
endif

if has("autocmd")
  augroup vimrcEx
    au!

    " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=108

    " Trim whitespace onsave
    autocmd BufWritePre * %s/\s\+$//e

    " When editing a file, always jump to the last known position.
    autocmd BufReadPost *
          \ if line("'\"") > 1 && line("'\"") <= line("$") |
          \   exe "normal! g`\"" |
          \ endif

  augroup END
endif " has("autocmd")

" tab stuff
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
set smartindent

" Use custom characters for tabstops and EOLs
set listchars=tab:▸\ ,eol:●

set complete-=i

set nrformats-=octal

set ttimeout
set ttimeoutlen=100

" detect .md as markdown instead of modula-2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Unix as standard file type
set ffs=unix,dos,mac

" Always utf8
set termencoding=utf-8
" set encoding=utf-8
set fileencoding=utf-8

set so=5 " scroll lines above/below cursor
set sidescrolloff=5
set lazyredraw

set magic " for regular expressions

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if has('path_extra')
  setglobal tags-=./tags tags^=./tags;
endif

set autoread

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif
if !empty(&viminfo)
  set viminfo^=!
endif
set sessionoptions-=options

" buffer settings
set hid " buffer becomes hidden when abandoned

" stop highlighting of underscores in markdown files
autocmd BufNewFile,BufRead,BufEnter *.md,*.markdown :syntax match markdownIgnore "_"

" clipboard
set clipboard=unnamedplus

" colorscheme
set background=dark
let base16colorspace=256
colorscheme base16-default-dark

" Syntax coloring lines that are too long
set synmaxcol=2048

" snippets
let g:UltiSnipsExpandTrigger="<c-j>""

" Disable syntax for some files
autocmd! bufreadpost *.min.js set syntax=off

" Change startup messsage
set shortmess+=I

" Ignore this files when search files
set wildignore+=.git/*,*/.git/*,*.DS_Store,*/node_modules/*,*/dist/*
set wildignore+=*/cache/*,*/.sass-cache/*
set wildignore+=*/coverage/*,*/public/*,*-min.js,*-build.js

" Make the command-line completion better
set wildmenu

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>rm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" netrw settings
let g:netrw_banner = 0
let g:netrw_browse_split = 0

" When the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8

set timeoutlen=500

set guioptions=ac

" Hide the mouse pointer while typing
set mousehide

" set visual bell
set novb

" wrapping off
set nowrap

let mapleader = " "                " set leader key for all shortcuts
set nocompatible                   " Disable compatibility to old-time vi
set showmatch                      " Show matching brackets.
set ignorecase                     " Do case insensitive matching
set mouse=v                        " middle-click paste with mouse
set hlsearch                       " highlight search results
set tabstop=2                      " number of columns occupied by a tab character
set softtabstop=2                  " see multiple spaces as tabstops so <BS> does the right thing
set expandtab                      " converts tabs to white space
set shiftwidth=2                   " width for autoindents
set autoindent                     " indent a new line the same amount as the line just typed
set number                         " add line numbers
set wildmode=longest,list          " get bash-like tab completions
set smartindent                    " smart indentation when starting a new line
set clipboard^=unnamed,unnamedplus " set clipboard
set shortmess+=I                   " startup message
set background=dark                " background theme color
set hidden
set wildignore+=.git/*,*/.git/*,*.DS_Store,*/node_modules/*,*/dist/*
set wildignore+=*/cache/*,*/.sass-cache/*
set wildignore+=*/coverage/*,*/public/*,*-min.js,*-build.js

" Keys

" toggle highlight search
nmap <silent> <leader>n :set invhls<cr>:set hls?<cr>

" Toggle paste mode
nmap <silent> <leader>i :set invpaste<cr>:set paste?<cr>

" set text wrapping toggles
nmap <silent> <leader>w :set invwrap<cr>:set wrap?<cr>

" Edit the vimrc file
nmap <leader>ev :e $MYVIMRC<cr>
nmap <leader>rv :so $MYVIMRC<cr>

" Make horizontal scrolling easier
nmap <silent> <C-h> 20zl
nmap <silent> <C-l> 20zh

" dont map s
nnoremap s <nop>

" Run latest vimux command
nmap t :VimuxRunLastCommand<cr>

" Save file
nmap <leader>s :w<cr>

" Plugins
"
let g:plug_shallow = 0
let g:plug_window  = 'enew'
let g:plug_pwindow = 'vertical rightbelow new'

call plug#begin('~/.config/nvim/plugged')

  " Ack
  Plug 'mileszs/ack.vim'

  let g:ackprg = 'ag --nogroup --nocolor --column'

  Plug 'gregsexton/gitv'
  nmap <leader>g :Gstatus<cr>

  Plug 'w0rp/ale'

  let g:ale_linters = {'javascript': ['eslint']}
  let g:ale_javascript_eslint_executable = './node_modules/.bin/eslint'
  let g:ale_sign_column_always = 1
  let g:ale_sign_error = '>'
  let g:ale_sign_warning = '-'
  let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']

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

  " CtrlP
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'

  nmap <C-p> :FZF<cr>

  " NERDTree
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeFocus' }

  let g:NERDTreeChDirMode = 2
  let g:NERDTreeMinimalUI = 1
  let g:NERDTreeIgnore=['node_modules', 'build', 'coverage']

  " Prevent deleting nerdtree buffer
  autocmd FileType nerdtree cnoreabbrev <buffer> bd :call g:WorkaroundNERDTreeToggle()<cr>

  " Shortcut for file explorer
  nmap <leader>f :NERDTreeFocus<cr>

  function! g:WorkaroundNERDTreeToggle()
    try | :NERDTreeToggle | catch | :enew | endtry
    echo ''
  endfunction

  " Styled Components
  Plug 'styled-components/vim-styled-components'

  " ReasonML
  Plug 'rgrinberg/vim-ocaml'
  au FileType ocaml nnoremap <C-n> <Esc>:FZFMerlinOutline<cr>

  Plug 'andreypopp/fzf-merlin'
  Plug 'reasonml-editor/vim-reason-plus'

  Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

  let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ }

  nnoremap <silent> gt :call LanguageClient#textDocument_typeDefinition()<cr>
  nnoremap <silent> gd :call LanguageClient#textDocument_definition()<cr>
  nnoremap <silent> gf :call LanguageClient#textDocument_formatting()<cr>
  nnoremap <silent> <cr> :call LanguageClient#textDocument_hover()<cr>
  nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<cr>

  set formatexpr=LanguageClient#textDocument_rangeFormatting()

  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

  " Prettier
  Plug 'prettier/vim-prettier', { 'do': 'npm install' }

  let g:prettier#exec_cmd_async = 1
  let g:prettier#autoformat = 0
  let g:prettier#quickfix_enabled = 0
  autocmd BufWritePre *.js,*.md PrettierAsync

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

  " Sayonara
  " Sane buffer/window deletion.

  Plug 'mhinz/vim-sayonara'

  nmap <silent> <leader>q :Sayonara<cr>

  " Easy Align
  Plug 'junegunn/vim-easy-align'

  " Start interactive EasyAlign in visual mode (e.g. vipga)
  xmap ga <Plug>(EasyAlign)

  " Start interactive EasyAlign for a motion/text object (e.g. gaip)
  nmap ga <Plug>(EasyAlign)

  colorscheme tir_black
call plug#end()

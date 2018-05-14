let mapleader = " "                                              " set leader key for all shortcuts
set encoding=utf-8                                               " Necessary to show unicode glyphs
set nocompatible                                                 " Disable compatibility to old-time vi
set showmatch                                                    " Show matching brackets.
set ignorecase                                                   " Do case insensitive matching
set smartcase
set hlsearch                                                     " highlight search results
set tabstop=2                                                    " number of columns occupied by a tab character
set softtabstop=2                                                " see multiple spaces as tabstops so <BS> does the right thing
set expandtab                                                    " converts tabs to white space
set shiftwidth=2                                                 " width for autoindents
set backspace=2                                                  " Allow backspacing over indent, eol, and the start of an insert
set autoindent                                                   " indent a new line the same amount as the line just typed
set number                                                       " add line numbers
set wildmode=longest,list                                        " get bash-like tab completions
set smartindent                                                  " smart indentation when starting a new line
set clipboard^=unnamed,unnamedplus                               " set clipboard
set shortmess+=I                                                 " startup message
set background=dark                                              " background theme color
set hidden
set wildignore+=.git/*,*/.git/*,*.DS_Store,*/node_modules/*      " ignore project related files
set wildignore+=*/dist/*,*/coverage/*                            " ignore dist and coverage files
set wildignore+=*-min.js,*-build.js                              " ignore minified files
set completeopt=longest,menuone
set nowrap
set shell=bash
set ch=1
set novb
set stl=%f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B] " Status line
set laststatus=2                                                 " always put a status line in, even if there is only one window
set complete=.,w,b,u,t,i,kspell                                  " Set where vim should look for aut completion
set showcmd                                                      " Show the current command in the lower right corner
set noshowmode                                                   " dont show current mode as its visible in lightline instead
set mousehide                                                    " hide mouse while typing
set mouse=a
set nobackup
set noswapfile
set autoread
set wmh=0                                                        " minimum window height
set listchars=tab:▸\ ,eol:●                                      " Use custom characters for tabstops and EOLs
set nolist                                                       " Dont show special characters by default
set dictionary=/usr/share/dict/words                             " Set dictionary (Its used with C-X C-K to autocomplete words)

" Set color scheme
colorscheme tir_black

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

  " Ale
  Plug 'w0rp/ale'
  let g:ale_linters = {'javascript': ['eslint']}
  let g:ale_javascript_eslint_executable = './node_modules/.bin/eslint'
  let g:ale_sign_column_always = 1
  let g:ale_sign_error = '✗'
  let g:ale_sign_warning = '▲'
  let g:ale_statusline_format = ['✗ %d', '▲ %d', '⬥ ok']
  let g:ale_lint_on_enter = 0
  let g:ale_set_highlights = 0

  nmap ]l :ALENextWrap<CR>
  nmap [l :ALEPreviousWrap<CR>

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
  let g:VimuxOrientation = "h"
  let g:VimuxUseNearestPane = 1

	" Run latest vimux command
	nmap t :VimuxRunLastCommand<cr>

  " CtrlP
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'

  nmap <C-p> :FZF<cr>

  " FZF color scheme updater from https://github.com/junegunn/fzf.vim/issues/59
  function! s:update_fzf_colors()
    let rules =
          \ { 'fg':      [['Normal',       'fg']],
          \ 'bg':      [['Normal',       'bg']],
          \ 'hl':      [['String',       'fg']],
          \ 'fg+':     [['CursorColumn', 'fg'], ['Normal', 'fg']],
          \ 'bg+':     [['CursorColumn', 'bg']],
          \ 'hl+':     [['String',       'fg']],
          \ 'info':    [['PreProc',      'fg']],
          \ 'prompt':  [['Conditional',  'fg']],
          \ 'pointer': [['Exception',    'fg']],
          \ 'marker':  [['Keyword',      'fg']],
          \ 'spinner': [['Label',        'fg']],
          \ 'header':  [['Comment',      'fg']] }
    let cols = []
    for [name, pairs] in items(rules)
      for pair in pairs
        let code = synIDattr(synIDtrans(hlID(pair[0])), pair[1])
        if !empty(name) && code != ''
          call add(cols, name.':'.code)
          break
        endif
      endfor
    endfor
    let s:orig_fzf_default_opts = get(s:, 'orig_fzf_default_opts', $FZF_DEFAULT_OPTS)
    let $FZF_DEFAULT_OPTS = s:orig_fzf_default_opts .
          \ (empty(cols) ? '' : (' --color='.join(cols, ',')))
  endfunction

  augroup _fzf
    autocmd!
    autocmd VimEnter,ColorScheme * call <sid>update_fzf_colors()
  augroup END

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
    \ 'do': 'bash install.sh && npm install -g javascript-typescript-langserver ocaml-language-server'
    \ }

  let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ }

  nnoremap <silent> gt :call LanguageClient#textDocument_typeDefinition()<cr>
  nnoremap <silent> gd :call LanguageClient#textDocument_definition()<cr>
  nnoremap <silent> gf :call LanguageClient#textDocument_formatting()<cr>
  nnoremap <silent> <cr> :call LanguageClient#textDocument_hover()<cr>
  nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<cr>

  set formatexpr=LanguageClient#textDocument_rangeFormatting()

  " Prettier
  Plug 'prettier/vim-prettier', { 'do': 'npm install' }

  let g:prettier#exec_cmd_async = 1
  let g:prettier#autoformat = 0
  let g:prettier#quickfix_enabled = 0
  autocmd BufWritePre *.js,*.md PrettierAsync

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

  " Utlisnips
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'

  let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips', 'UltiSnips']
  let g:UltiSnipsEditSplit="vertical"

  " Javascript Statical Analysis
  Plug 'ternjs/tern_for_vim'

  " Deoplete Async completetion framework
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

  " Deoplete ternjs
  Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
  let g:tern#command = ["tern"]
  let g:tern#arguments = ["--persistent"]

  " JavaScript Parameter Complete
  Plug 'othree/jspc.vim'

  " Javascript/React
  Plug 'pangloss/vim-javascript'
  Plug 'mxw/vim-jsx'

  " Vim status line
  Plug 'itchyny/lightline.vim'
  Plug 'edkolev/tmuxline.vim'

  let g:lightline = {
    \ 'colorscheme': 'wombat',
    \ 'active': {
    \   'left': [['mode', 'paste'], ['filename', 'modified']],
    \   'right': [['lineinfo'], ['percent'], ['readonly', 'linter_warnings', 'linter_errors', 'linter_ok']]
    \ },
    \ 'component_expand': {
    \   'linter_warnings': 'LightlineLinterWarnings',
    \   'linter_errors': 'LightlineLinterErrors',
    \   'linter_ok': 'LightlineLinterOK'
    \ },
    \ 'component_type': {
    \   'readonly': 'error',
    \   'linter_warnings': 'warning',
    \   'linter_errors': 'error'
    \ },
    \ }

  function! LightlineLinterWarnings() abort
    let l:counts = ale#statusline#Count(bufnr(''))
    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors
    return l:counts.total == 0 ? '' : printf('%d ◆', all_non_errors)
  endfunction

  function! LightlineLinterErrors() abort
    let l:counts = ale#statusline#Count(bufnr(''))
    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors
    return l:counts.total == 0 ? '' : printf('%d ✗', all_errors)
  endfunction

  function! LightlineLinterOK() abort
    let l:counts = ale#statusline#Count(bufnr(''))
    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors
    return l:counts.total == 0 ? '✓ ' : ''
  endfunction

  " Update and show lightline but only if it's visible (e.g., not in Goyo)
  autocmd User ALELint call s:MaybeUpdateLightline()
  function! s:MaybeUpdateLightline()
    if exists('#lightline')
      call lightline#update()
    end
  endfunction

  if !has('gui_running')
    set t_Co=256
  endif
call plug#end()

" CSS
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" Bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" Remap arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Special characters

" Invisible character(tabstops, EOLs) custom color
highlight NonText guifg=#124956

" Toggle list, showing or hiding special chars
nmap <leader>l :setlocal list!<cr>


" netrw settings
let g:netrw_banner = 0
let g:netrw_browse_split = 0

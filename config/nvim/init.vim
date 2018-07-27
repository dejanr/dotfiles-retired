let mapleader = " "                                              " set leader key for all shortcuts
set autoindent                                                   " indent a new line the same amount as the line just typed
set autoread
set backspace=indent,eol,start                                   " Make backspace sane.<Paste>
set ch=1
set clipboard^=unnamed,unnamedplus                               " set clipboard
set complete=.,w,b,u,t,i,kspell                                  " Set where vim should look for aut completion
set completeopt=longest,menuone
set dictionary=/usr/share/dict/words                             " Set dictionary (Its used with C-X C-K to autocomplete words)
set encoding=utf-8                                               " Necessary to show unicode glyphs
set expandtab                                                    " converts tabs to white space
set hidden
set hlsearch                                                     " highlight search results
set ignorecase                                                   " Do case insensitive matching
set laststatus=2                                                 " always put a status line in, even if there is only one window
set listchars=tab:▸\ ,eol:●                                      " Use custom characters for tabstops and EOLs
set mouse=a
set mousehide                                                    " hide mouse while typing
set nobackup
set nocompatible                                                 " Disable compatibility to old-time vi
set nolist                                                       " Dont show special characters by default
set noshowmode                                                   " dont show current mode as its visible in lightline instead
set noswapfile
set novb
set nowrap
set number                                                       " add line numbers
set shell=bash
set shiftwidth=2                                                 " width for autoindents
set shortmess+=I                                                 " startup message
set showcmd                                                      " Show the current command in the lower right corner
set showmatch                                                    " Show matching brackets.
set smartcase
set smartindent                                                  " smart indentation when starting a new line
set softtabstop=2                                                " see multiple spaces as tabstops so <BS> does the right thing
set stl=%f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B] " Status line
set tabstop=2                                                    " number of columns occupied by a tab character
set wildignore+=*-min.js,*-build.js                              " ignore minified files
set wildignore+=*/dist/*,*/coverage/*                            " ignore dist and coverage files
set wildignore+=.git/*,*/.git/*,*.DS_Store,*/node_modules/*      " ignore project related files
set wildmode=longest,list                                        " get bash-like tab completions
set wmh=0                                                        " minimum window height
set termguicolors                                              " Use real colors.
set signcolumn=yes                                               " Sign column always on
set background=dark

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
  " Git 
  Plug 'gregsexton/gitv'
  nmap <leader>g :Gstatus<cr>

  " Tim Pope
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-surround'
  Plug 'Raimondi/delimitMate'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-speeddating'

  " Ale
  Plug 'w0rp/ale'
  let g:ale_linters = {'javascript': ['eslint']}
  let g:ale_javascript_eslint_executable = './node_modules/.bin/eslint'
  let g:ale_sign_error = 'x'
  let g:ale_sign_warning = '▲'
  let g:ale_sign_column_always = 1
  let g:ale_statusline_format = ['✗ %d', '▲ %d', '⬥ ok']
  let g:ale_lint_on_enter = 1
  let g:ale_set_highlights = 1

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

  " --column: Show column number
  " --line-number: Show line number
  " --no-heading: Do not show file headings in results
  " --fixed-strings: Search term as a literal string
  " --ignore-case: Case insensitive search
  " --no-ignore: Do not respect .gitignore, etc...
  " --hidden: Search hidden files and folders
  " --follow: Follow symlinks
  " --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
  " --color: Search color options
  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

  " NERDTree
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeFocus' }

  let g:NERDTreeChDirMode = 2
  let g:NERDTreeMinimalUI = 1
  let g:NERDTreeIgnore=['node_modules', 'build', 'coverage']
  let NERDTreeShowHidden=1
  let NERDTreeDirArrowExpandable = '▷'
  let NERDTreeDirArrowCollapsible = '▼'
  let g:NERDTreeIndicatorMapCustom = {
  \ "Modified"  : "✹",
  \ "Staged"	  : "✚",
  \ "Untracked" : "✭",
  \ "Renamed"   : "➜",
  \ "Unmerged"  : "═",
  \ "Deleted"   : "✖",
  \ "Dirty"	  : "✗",
  \ "Clean"	  : "✔︎",
  \ 'Ignored'   : '☒',
  \ "Unknown"   : "?"
  \ }

  set grepprg=rg\ --vimgrep

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
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
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

  " Deoplete Async completetion framework
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

  " JavaScript Parameter Complete
  Plug 'othree/jspc.vim'

  " Javascript/React
  Plug 'pangloss/vim-javascript'
  Plug 'mxw/vim-jsx'

  " Dont require jsx file extension for enabling this plugin
  let g:jsx_ext_required = 0

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

  " Syntax Attribute Checker
  Plug 'vim-scripts/SyntaxAttr.vim'
  nmap <leader>c	:call SyntaxAttr()<CR>

  " Base-16
  Plug 'Soares/base16.nvim'

	" GraphQL
  Plug 'jparise/vim-graphql'
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

" ============================================================================
" Color management ===========================================================
let g:base16_transparent_background = 0

" Changes to the base theme
let g:base16_color_modifiers = {
      \ 'ErrorMsg': 'fg=red bg=none none',
      \ 'Comment': 'fg=green'}

" Hacks to prevent me from writing my own syntax files
let g:base16_color_overrides = {
      \ 'vimCommentTitle': 'fg=yellow italic',
      \ 'ALEErrorSign': 'fg=red bg=similar3 bold',
      \ 'ALEWarningSign': 'fg=orange bg=similar3 bold',
      \ 'fzf1': 'fg=red bg=similar2',
      \ 'fzf2': 'fg=contrast1 bg=similar2',
      \ 'fzf3': 'fg=contrast2 bg=similar2',
      \ 'GitGutterAdd': 'fg=green bg=similar3',
      \ 'GitGutterChange': 'fg=yellow bg=similar3',
      \ 'GitGutterDelete': 'fg=red bg=similar3',
      \ 'GitGutterChangeDelete': 'fg=orange bg=similar3'}

colorscheme default

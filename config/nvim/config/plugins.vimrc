filetype plugin indent on

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#disable_auto_complete = 0

let g:deoplete#omni#functions = {}
let g:deoplete#omni#functions.javascript = [
  \ 'tern#Complete',
  \ 'jspc#omni'
\]

set completeopt=longest,menuone,preview
let g:deoplete#sources = {}
let g:deoplete#sources['javascript.jsx'] = ['file', 'ultisnips', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent']
set completeopt-=preview " disable preview for completes

autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

if !exists('g:deoplete#omni#input_patterns')
  let g:deoplete#omni#input_patterns = {}
endif

" neomake config
autocmd! BufWritePost * Neomake

let g:neomake_warning_sign = {
  \ 'text': 'W',
  \ 'texthl': 'WarningMsg',
  \ }

let g:neomake_error_sign = {
  \ 'text': 'E',
  \ 'texthl': 'ErrorMsg',
  \ }

let g:neomake_open_list = 0

let g:neomake_javascript_enabled_makers = ['eslint']

" make background transparent
hi Normal ctermbg=NONE
hi EndOfBuffer ctermbg=NONE
hi LineNr ctermbg=234

" javascript libraries syntax
let g:used_javascript_libs = 'jquery,underscore,react,flux,chai,redux,sinon'

" completion
augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end

" tern
if exists('g:plugs["tern_for_vim"]')
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1

  autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

" disable colorizer at startup
let g:colorizer_startup = 0
let g:colorizer_nomap = 1

" ack
let g:ackprg = 'ag --nogroup --nocolor --column'

" Gist
let g:gist_detect_filetype = 1
let g:gist_show_privates = 1
let g:gist_get_multiplefile = 1
let g:gist_clip_command = 'pbcopy'
let g:gist_browser_command = '/usr/bin/open -a "/Applications/Google Chrome.app" "%URL%"'

" CtrlP
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 1
let g:ctrlp_working_path_mode = ''

nmap <leader>b :CtrlPBuffer<CR>

" Vimux

let g:VimuxOrientation = "h"

" Initialize ctrlp delete buffer from autoload (there is no bundle plugin yet)
" Use C-@ to delete buffer
call ctrlp_bdelete#init()

if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" NERDTree
let g:NERDTreeChDirMode = 2
let g:NERDTreeMinimalUI = 1

" Prevent deleting nerdtree buffer
autocmd FileType nerdtree cnoreabbrev <buffer> bd :call g:WorkaroundNERDTreeToggle()<CR>

function! g:WorkaroundNERDTreeToggle()
  try | :NERDTreeToggle | catch | :enew | endtry
  echo ''
endfunction

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

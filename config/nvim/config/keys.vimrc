" map Leader
let mapleader = " "

" in-line scrolling
nmap <Leader>j gj
nmap <Leader>k gk

" Bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" No arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" File explorer
nmap <leader>f :NERDTreeFocus<CR>

" Ctrlp list buffers
nmap <leader>b :CtrlPBuffer<CR>

map <silent> <leader>d :bd<CR>
map <silent> <leader>D :bd!<CR>

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
nmap <silent> <C-o> 20zl
nmap <silent> <C-i> 20zh

nmap <silent> <leader>q :q!<CR>

" Toggle spell checking
map <leader>ss :setlocal spell!<cr>

" Toggle list, showing or hiding special chars
nmap <leader>l :setlocal list!<cr>

" Run last vimux command
nmap t :VimuxRunLastCommand<CR>

" Save file
nmap <leader>s :w<CR>

" snippets
let g:UltiSnipsExpandTrigger="<c-j>"

" remap number increment to C-a
nmap <C-s> <C-a>

" override read-only permissions
cmap w!! %!sudo tee > /dev/null %

" start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Quickly open a scratch buffer
map <leader>oq :e ~/.scratch-buffer<cr>:w<cr>

" Toggle undotree
nnoremap <leader>u :UndotreeToggle<cr>

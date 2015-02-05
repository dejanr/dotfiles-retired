if has("gui_macvim")
  macmenu &File.Close\ Window key=<nop>
  macmenu &File.Close key=<nop>
  macmenu &File.New\ Tab key=<nop>
  macmenu &Tools.Make key=<nop>

  nmap <D-e> :CtrlPBufTag<CR>
  imap <D-e> <Esc>:CtrlPBufTag<CR>
  nmap <D-r> :CommandT<CR>
  imap <D-r> <Esc>:CommandT<CR>
  nmap <D-t> :CommandTTag<CR>
  imap <D-t> <Esc>:CommandTTag<CR>
  nmap <D-b> :CommandTBuffer<CR>
  imap <D-b> <Esc>:CommandTBuffer<CR>

  " Set window size to maximum
  set guifont=Monaco:h13
  set lines=120 columns=310
endif

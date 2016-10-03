let s:bundle_dir   = '~/.config/nvim/plugged'
let g:plug_shallow = 0
let g:plug_window  = 'enew'
let g:plug_pwindow = 'vertical rightbelow new'

call plug#begin(s:bundle_dir)

" colorschemes
Plug 'chriskempson/base16-vim'

" general
Plug 'benekastah/neomake' " Asynchronous :make using Neovim's job-control functionality
Plug 'Shougo/deoplete.nvim' " Asynchronous completion framework for neovim
Plug 'carlitux/deoplete-ternjs', { 'for': ['javascript', 'javascript.jsx'] } " deoplete.nvim source for javascript
Plug 'scrooloose/nerdtree' " A tree explorer
Plug 'SirVer/ultisnips' " The ultimate snippet
Plug 'honza/vim-snippets' " Default snippets for ultisnips
Plug 'haya14busa/incsearch.vim' " Improved incremental searching
Plug 'kien/ctrlp.vim' " Fuzzy file, buffer, mru, tag, etc finder
Plug 'christoomey/vim-tmux-navigator' " Seamless navigation between tmux panes and vim splits<Paste>

" editing
Plug 'junegunn/vim-easy-align' " A Vim alignment plugin
Plug 'mbbill/undotree' " The ultimate undo history visualizer
Plug 'tpope/vim-commentary' " Comment stuff out
Plug 'airblade/vim-gitgutter' " Shows a git diff in the gutter
Plug 'nathanaelkane/vim-indent-guides' " Visually displaying indent levels `<leader>ig` to toggle
Plug 'Raimondi/delimitMate' " Provides insert mode auto-completion for quotes, parens, brackets
Plug 'tpope/vim-repeat' " Enable repeating supported plugin maps with .
Plug 'tpope/vim-speeddating' " Use CTRL-A/CTRL-X to increment dates, times, and more
Plug 'tpope/vim-surround' " Quoting and parenthesizing made simple
Plug 'tpope/vim-unimpaired' " Pairs of handy bracket mappings
Plug 'justinmk/vim-sneak' " The missing motion
Plug 'vim-scripts/camelcasemotion' " Motion through CamelCaseWords and underscore_notation
Plug 'sheerun/vim-polyglot' " A solid language pack

" eye candy
Plug 'vim-airline/vim-airline' " lean & mean status/tabline for vim that's light as air
Plug 'vim-airline/vim-airline-themes' " A collection of themes for vim-airline
Plug 'lilydjwg/colorizer', { 'on': 'ColorToggle' } " Colorize all text in the form #rrggbb or #rg

" javascript
Plug 'guileen/vim-node-dict' " node.js dict for vim
Plug 'moll/vim-node' " Tools and environment to make Vim superb for developing with Node.js
Plug 'othree/yajs.vim', { 'for': 'javascript' } " Yet Another JavaScript Syntax
Plug 'othree/javascript-libraries-syntax.vim' " Syntax for JavaScript libraries
Plug '1995eaton/vim-better-javascript-completion' " An expansion of Vim's current JavaScript syntax file
Plug 'gavocanov/vim-js-indent' " JavaScript indentation
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' } " Tern plugin
Plug 'othree/jspc.vim' " JavaScript Parameter Complete
Plug 'digitaltoad/vim-jade' " Jade template engine syntax highlighting and indention
Plug 'elzr/vim-json' " Distinct highlighting of keywords vs values, JSON-specific (non-JS) warnings, quote concealing. Pathogen-friendly
Plug 'mxw/vim-jsx' " React JSX syntax highlighting and indenting
Plug 'jparise/vim-graphql' " GraphQL file detection and syntax highlighting

" elixir
Plug 'elixir-lang/vim-elixir' " Elixir support

" other
Plug 'mattn/emmet-vim' " Provides support for expanding abbreviations similar to emmet
Plug 'othree/html5.vim' " HTML5 omnicomplete and syntax
Plug 'hail2u/vim-css3-syntax' " CSS3 syntax support to vim's built-in `syntax/css.vim`
Plug 'othree/csscomplete.vim' " Update the bult-in CSS complete function to latest CSS standard
Plug 'groenewege/vim-less' " Syntax for LESS
Plug 'benmills/vimux' " vim plugin to interact with tmux
Plug 'mileszs/ack.vim' " Search tool
Plug 'gitv' " Git history browser
Plug 'tpope/vim-fugitive'

" Gist
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'

" text objects
Plug 'wellle/targets.vim' " Provides additional text objects
Plug 'kana/vim-textobj-user' " Create your own text objects
Plug 'glts/vim-textobj-comment' " Text objects for comments
Plug 'kana/vim-textobj-fold' " Text objects for foldings
Plug 'kana/vim-textobj-indent' " Text objects for indented blocks of lines
Plug 'kana/vim-textobj-function' " Text objects for functions

" Bash
Plug 'bash-support.vim'

call plug#end()

" Set line numbers, tabstops, autoindentation, etc.
set number
" set relativenumber
set tabstop=8
set shiftwidth=8
set expandtab
set ai
set si
set cindent
set mouse=a
set foldmethod=indent

" Plugins will be downloaded under the specified directory.
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

" Declare the list of plugins.
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'preservim/nerdtree'
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}
Plug 'ahmedkhalf/project.nvim'
Plug 'tpope/vim-sensible'
Plug 'junegunn/seoul256.vim'
Plug 'rust-lang/rust.vim'
Plug 'alaviss/nim.nvim'
Plug 'ggandor/leap.nvim'
" Plug 'tpope/vim-repeat.vim'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()

lua require('leap').create_default_mappings()

inoremap <expr> <cr> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"
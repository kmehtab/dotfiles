" auto-install vim-plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')

    " Better Comments
    Plug 'tpope/vim-commentary'
    "Gruvbox
    Plug 'gruvbox-community/gruvbox'
    " Better Syntax Support
    Plug 'sheerun/vim-polyglot'
    " Auto pairs for '(' '[' '{'
    Plug 'jiangmiao/auto-pairs'

    Plug 'joshdick/onedark.vim'

    " Stable version of coc
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    
    " You Complete me
    "Plug 'ycm-core/YouCompleteMe'

    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    
    " Ranger
    Plug 'kevinhwang91/rnvimr', {'do': 'make sync'}

    Plug 'norcalli/nvim-colorizer.lua'

    Plug 'junegunn/rainbow_parentheses.vim'

    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'junegunn/rainbow_parentheses.vim'

    "VIM Snippets for all lang
    Plug 'honza/vim-snippets'

    " Vim Fugitive
    Plug 'tpope/vim-fugitive'
    
    "Start screen
    Plug 'mhinz/vim-startify'
    " live server
    Plug 'turbio/bracey.vim'

    "Devicons 
    Plug 'ryanoasis/vim-devicons'
    call plug#end()

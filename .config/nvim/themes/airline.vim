" enable tabline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
 
" Just show the file name
let g:airline#extensions#tabline#fnamemod = ':t'

"enable/disable vim-capslock integration >
let g:airline#extensions#capslock#enabled = 1

  " enable powerline fonts
let g:airline_powerline_fonts = 1

" Switch to your current theme
let g:airline_theme = 'gruvbox'

" Always show tabs
set showtabline=2

" We don't need to see things like -- INSERT -- anymore
set noshowmode

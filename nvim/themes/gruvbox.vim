syntax on
let g:gruvbox_contrast_dark = 'hard'
colorscheme gruvbox
set background=dark
hi normal guibg=000000

" checks if your terminal has 24-bit color support
if (has("termguicolors"))
    set termguicolors
    hi LineNr ctermbg=NONE guibg=NONE
endif

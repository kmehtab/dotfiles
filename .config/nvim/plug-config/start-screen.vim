let g:startify_custom_header = [
\'        $$\   $$\                           $$\    $$\ $$\               ',
\'        $$$\  $$ |                          $$ |   $$ |\__|              ',
\'        $$$$\ $$ | $$$$$$\   $$$$$$\        $$ |   $$ |$$\ $$$$$$\$$$$\  ',
\'        $$ $$\$$ |$$  __$$\ $$  __$$\       \$$\  $$  |$$ |$$  _$$  _$$\ ',
\'        $$ \$$$$ |$$$$$$$$ |$$ /  $$ |       \$$\$$  / $$ |$$ / $$ / $$ |',
\'        $$ |\$$$ |$$   ____|$$ |  $$ |        \$$$  /  $$ |$$ | $$ | $$ |',
\'        $$ | \$$ |\$$$$$$$\ \$$$$$$  |         \$  /   $$ |$$ | $$ | $$ |',
\'        \__|  \__| \_______| \______/           \_/    \__|\__| \__| \__|',
\ ]
let g:startify_session_dir = '~/.config/nvim/session'
let g:startify_lists = [
          \ { 'type': 'files',     'header': ['   Files']            },
          \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
          \ { 'type': 'sessions',  'header': ['   Sessions']       },
          \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
          \] 
let g:startify_bookmarks = [
            \ { 'i': '~/.config/nvim/init.vim' },
            \ { 'z': '~/.zshrc' },
            \ { 'p': '/Volumes/Study\ HD/Development/projects'},
            \ { 'js': '/Volumes/Study\ HD/Development/projects/complete-javascript-course'},
            \ ]
let g:startify_session_autoload = 1
let g:startify_session_delete_buffers = 1
let g:startify_change_to_vcs_root = 1
let g:startify_enable_special = 0

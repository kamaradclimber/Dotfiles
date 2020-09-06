" test

" Mandatory specification of plugins directory
call plug#begin(stdpath('data') . '/plugged')


" linting (alternative to syntastic)
Plug 'neomake/neomake'
Plug 'janko-m/vim-test' "test for ruby
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'peitalin/vim-jsx-typescript' "typescript
Plug 'ActivityWatch/aw-watcher-vim'

" Initialize plugin system
call plug#end()

call neomake#configure#automake('nrwi', 500)


let mapleader=","

set ignorecase  "Ignore case when searching
set number " line numbers

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

":W ask for sudo password to save the file
command W w !sudo tee % > /dev/null

" vim-test setup
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>

set tabstop=2       " number of visual spaces per TAB
set softtabstop=2   " number of spaces in tab when editing
set shiftwidth=2    " number of spaces to use for autoindent
set expandtab       " tabs are space
set autoindent
set copyindent      " copy indent from the previous line

colorscheme 256-jungle

set nocompatible " Less vi-compatible, has a lot of side-effects so should be at the beginning


":W ask for sudo password to save the file
command W w !sudo tee % > /dev/null  


set incsearch " Search as you type

set hlsearch " Highlight search results

set smartcase " Ignore case when searching if all min letters. If one letter is uppercase, then don't ignore the case


syntax on " Basic syntax coloration


filetype on  " Auto detection of filetype

" Load the default behavior for some filetypes such as mail, commit message,
" changelog,...
filetype plugin on

filetype indent on " Load indentation depending on the filetype

set autoread " Reload files that have changed



set smartindent " Automatic indentation of code, type :help smartindent to have more information. Maybe some plugins do very clever indentation (language dependant)

set expandtab " Expands tabs to spaces

set shiftwidth=4 " Indentation is 4 spaces

" Use of <tab> in front of a line will insert shifwidth spaces, evrywhere
" else it will insert tabstop (by default 8 spaces)
set smarttab


set showcmd "displays the number of lines selected in visual mode and the key pressed in cmd mode.


set mouse="a" " Mouse handling
set mousefocus "the mouse focus when using splitted buffers
set mousemodel=extend "not too bad use of the mouse


set ruler " Show the position of cursor



set background&  "adapt background automatically.

" Set comments color to more visible
"highlight Comment ctermbg=DarkGray

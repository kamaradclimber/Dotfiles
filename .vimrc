
" Less vi-compatible, has a lot of side-effects so should be at the beginning
set nocompatible


" :W ask for sudo password to save the file
command W w !sudo tee % > /dev/null


" Search as you type
set incsearch

" Highlight search results
set hlsearch

" Ignore case when searching if all min letters. If one letter is uppercase,
" then don't ignore the case
set smartcase

" Set magic mode. $,.,$ are interpreted. \(,\) need to be used for group. For
" complete explaination :h magic
set magic


" Basic syntax coloration
syntax on


" Auto detection of filetype
filetype on 

" Load the default behavior for some filetypes such as mail, commit message,
" changelog,...
filetype plugin on

" Load indentation depending on the filetype
filetype indent on

" Reload files that have changed
set autoread


" Completion : List all the matches and complete to longest common prefix
set wildmode="list:longest"


" Show matching bracklet
set showmatch


" Display 7 lines below the cursor
set scrolloff=7


" Display line numbers
:set number "replaced temporarly by relativenumber

" Display line numbers relatively to the current line
"set relativenumber

" Automatic indentation of code, type :help smartindent to have more information. Maybe some plugins do very clever indentation (language dependant)
set smartindent

" Expands tabs to spaces
set expandtab

" Indentation is 4 spaces
set shiftwidth=4

" Use of <tab> in front of a line will insert shifwidth spaces, evrywhere
" else it will insert tabstop (by default 8 spaces)
set smarttab




" Allow to toggle paste mode with the F3 command. Useful if you don't want to have the paste txt auto (ill) idented
set pastetoggle=<F3> " does not seem to work TODO


" Mouse handling
set mouse="a"
set mousefocus "the mouse focus when using splitted buffers
set mousemodel=extend "not too bad use of the mouse


" Ruler
set ruler
set laststatus=2 " Always display the status bar

" Set all the language shortcuts used by Vim to speal to the user. Thou shall read the doc about this !
set shortmess=aT " alls abbreviations and truncat the middle of long messages

set background="dark"


" Set comments color to more visible
highlight Comment ctermbg=DarkGray



" Git branch
function! GitBranch()
	let branch = system("git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* //'")
	if branch != ''
	return '   Git Branch: ' . substitute(branch, '\n', '', 'g')
	en
	return ''
	endfunction


function! HasPaste()
	if &paste
	return 'PASTE MODE  '
	en
	return ''
	endfunction

	" Format the statusline
	set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l/%L%{GitBranch()}

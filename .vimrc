"silent! colorscheme desert

set history=300
set nocompatible

":W ask for sudo password to save the file
command W w !sudo tee % > /dev/null  

filetype plugin indent on

set t_Co=256
syntax enable
set background&  "adapt background automatically.

augroup filetypedetect 
    autocmd FileType ruby,eruby,yaml set sw=2 sts=2
    autocmd FileType java,sh,haskell set sw=2 sts=2
augroup END 

set autoread

" Allow backspacing anytime
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set ignorecase  "Ignore case when searching
set hlsearch    "Highlight search things
set incsearch   "Search as you type
set magic       "Set magic on, for regular expressions
set smartcase   "Ignore case when searching if all min letters. If one letter is uppercase, then don't ignore the case
set showmatch   "Show matching bracets when text indicator is over them
set mat=2       "How many tenths of a second to blink

" No sound on errors
set noerrorbells
set visualbell

set encoding=utf8
try
    lang en_US
catch
endtry
set ffs=unix,dos,mac "Default file types

" Tabs as space of 4 (by default)
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4


" Auto-wrap comments and allow "gq" formatting
" Trailing white space indicates a paragraph continues in the next line. 
" A line that ends in a non-white character ends a paragraph.
set formatoptions=cqw

" Autoindent and wrap lines
set autoindent
set smartindent
set wrap "only change text display
set linebreak "only change text display (line break are smarter)
"set textwidth=80 "actually breaks line

" Line numbers
set number

" Status line
set ruler           "Always show current position
set cmdheight=2     "The commandbar height
set showcmd         "Show command being typed at the bottom
set laststatus=2    "Always show the statusline
" Format the statusline
set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c

function! CurDir()
    let curdir = substitute(getcwd(), "/home/grego", "~/", "g")
    return curdir
endfunction

if !empty(system("setxkbmap -print|grep bepo"))
    " movement keys
    noremap c h
    noremap r l
    noremap t j
    noremap s k
    noremap C H
    noremap R L
    noremap T J
    noremap S K
    " easier page up/down 
    noremap <BS> <PageUp>
    noremap <Space> <PageDown>
    " enter to center cursor
    noremap <Return> zz
endif

autocmd BufRead /tmp/mutt*  :source ~/.mail.vim

" Highlight trailing whitespaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

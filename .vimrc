set history=300
set nocompatible

" vundle setup require filetype to be off during setup
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'janko-m/vim-test' "test for ruby
Plugin 'derekwyatt/vim-scala' "scala stuff
Plugin 'bogado/file-line' "allow to open filename:line
Plugin 'w0rp/ale' " linting
Plugin 'sheerun/vim-polyglot' " correct indentation for ruby and other languages
Plugin 'neoclide/coc.nvim', {'pinned': 1 }
" all plugins should be set above
call vundle#end()

" reactivate filetype after vundle setup
filetype plugin indent on


":W ask for sudo password to save the file
command W w !sudo tee % > /dev/null

" visual autocomplete for command menu
set wildmenu

set t_Co=256
syntax enable
set background&  "adapt background automatically.

augroup filetypedetect
    autocmd FileType ruby,eruby,yaml,xml set sw=2 sts=2
    autocmd FileType java,sh,haskell set sw=2 sts=2
    autocmd FileType javascript set sw=2 sts=2
    autocmd FileType python set sw=2 sts=2
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

set tabstop=4     " display tabs as 4 spaces
set softtabstop=4 " number of spaces when adding tabs
set shiftwidth=4  "
set expandtab     " tabs are converted to spaces
set smarttab


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
    " easier page up/down
    noremap <BS> <PageUp>
    noremap <Space> <PageDown>
    " enter to center cursor
    noremap <Return> zz
endif

autocmd BufRead /tmp/mutt*  :source ~/.dotfiles/mutt/mail.vim
autocmd BufRead *thrift :set syntax=thrift

" Highlight trailing whitespaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

let mapleader=","

silent! colorscheme desert

" vim-test setup
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>

set statusline+=%#warningmsg#
set statusline+=%*

let b:ale_fixers = {'ruby': ['rubocop']}
let b:ale_ruby_rubocop_executable = 'bundle'

au FileType qf call AdjustWindowHeight(1, 4)
function! AdjustWindowHeight(minheight, maxheight)
  exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

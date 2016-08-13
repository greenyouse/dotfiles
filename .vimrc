" VIMRC 4 Me! :)
" Maintainer: Greenyouse
"


" General Settings"{{{

" Leaders
let mapleader=","
let maplocalleader="\\"

" matchit
runtime! macros/matchit.vim

" For local configuration files
set exrc
set secure

" Allow these file extensions to be accessed via 'gf' of only the name, for
" e.g. gf on [[AnotherPage]] should go to AnotherPage.pd
set suffixesadd=.pd,.txt

" Assume Bash is my shell (:help sh.vim)
let g:is_bash = 1

" Default color scheme
set background=dark

" Local config
let vimrc_local = expand("~/.vimrc.local", ":p")
if filereadable(vimrc_local)
    execute 'source' vimrc_local
endif
unlet vimrc_local

" Pathogen plugin
execute pathogen#infect()

set nonumber

" Bundle 'tpope/vim-fugitive'
" http://vimcasts.org/blog/2011/05/the-fugitive-series/
autocmd BufReadPost fugitive://* set bufhidden=delete

" Reformatting options. See `:help fo-table`
set formatoptions+=lnor1

" Disable spellcheck by default
set nospell

" Show line number, cursor position.
set ruler

" Display incomplete commands.
set showcmd

" Search as you type.
set incsearch

" Better scroll rendering
set ttyfast

" Ignore case while searching
set ignorecase

" Make /g flag default when doing :s
set gdefault

" Show autocomplete menus
set wildmenu

" Show editing mode
set showmode

" Ignore certain filetypes when doing filename completion
set wildignore=*.sw*,*.pyc,*.bak

" Show matching brackets
set showmatch

" Bracket blinking
set matchtime=2

" Split new window below current one
set splitbelow

" Error bells are displayed visually.
set visualbell

" Automatically read files which have been changed outside of Vim, if we
" haven't changed it already.
set autoread

" Disable highlighting after search. Too distracting.
set nohlsearch
" Enable syntax highlighting.
syntax on

" Line endings should be Unix-style unless the file is from someone else.
set fileformat=unix
au BufNewFile * set fileformat=unix

" Automatically indent when adding a curly bracket, etc.
" required! by vundle
filetype plugin indent on
set autoindent
set smartindent

" Tabs converted to 4 spaces
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab
set backspace=indent,eol,start

" Show special characters
if v:version >= 700
    set list listchars=tab:>-,trail:.,extends:>,nbsp:_
else
    set list listchars=tab:>-,trail:.,extends:>
endif

" Don't break up long lines, but visually wrap them.
set textwidth=0
set wrap

" Don't highlight lines longer than 800 chars
set synmaxcol=800

" Time out on key codes but not mappings.
set notimeout
set ttimeout
set ttimeoutlen=10

" Skip backup for crons
set backupskip=/tmp/*,/private/tmp/*"

" Set up backup dir where the swap files are stored
" set dir=~/.vim/backup
" set backupdir=~/.vim/backup

" Watch out!  This might gobble up lots of memory if there
" are many buffers + excessive amounts of undos!
" set undofile

"}}}
" Visual Things"{{{

" Status line
set laststatus=2
set statusline=
set statusline+=%-3.3n\                         " buffer number
set statusline+=%f\                             " filename
set statusline+=%h%m%r%w                        " status flags
if isdirectory(expand("~/.vim/bundle/vim-fugitive", ":p"))
    set statusline+=%{fugitive#statusline()}        " git status
endif
if isdirectory(expand("~/.vim/bundle/syntastic", ":p"))
    set statusline+=%{SyntasticStatuslineFlag()}    " syntastic status - makes sense with :Errors
endif
set statusline+=\[%{strlen(&ft)?&ft:'none'}]    " file type
set statusline+=%=                              " right align remainder
" set statusline+=0x%-8B                          " character value
set statusline+=%-14(%l,%c%V%)                  " line, character
set statusline+=%<%P                            " file position

" For GVIM
set guitablabel=%{GuiTabLabel()}\ %t

" Minimal number of screen lines to keep above and below the cursor.
" This keeps the cursor always in the vertical middle of the screen.
set scrolloff=999

"}}}
" Autocmds"{{{

" Commenting
augroup commentgroup
    autocmd!
    autocmd FileType javascript nnoremap <buffer> <LocalLeader>cm I// <esc>
    autocmd FileType python nnoremap <buffer> <LocalLeader>cm I# <esc>
    autocmd FileType clojure nnoremap <buffer> <LocalLeader>cm I; <esc>
    autocmd FileType lisp nnoremap <buffer> <LocalLeader>cm I;; <esc>
    autocmd FileType vim nnoremap <buffer> <LocalLeader>cm I" <esc>
augroup END

" Code folding 4 VimScripts!
augroup filetype_vim
    autocmd!
    autocmd FileType vim,clojure,clojurescript setlocal foldmethod=marker
augroup END

augroup FileTypes
    autocmd!
    " Ruby
    autocmd BufRead,BufNewFile {Gemfile,Rakefile,config.ru} setlocal ft=ruby
    autocmd FileType ruby setlocal tabstop=2 shiftwidth=2
    " JSON
    "autocmd BufRead,BufNewFile *.json setlocal ft=json foldmethod=syntax
    autocmd FileType json setlocal tabstop=2 shiftwidth=2
augroup END

" Clojure autocmds {{{

" Little Clojure helpers for vim practice
augroup Clojure
    autocmd!
    autocmd FileType clojurescript,clojure setlocal tabstop=2 shiftwidth=2
    autocmd FileType clojurescript,clojure :NeoComplCacheEnable
    autocmd FileType clojurescript,clojure nnoremap <buffer> <LocalLeader>yf i(defn
    autocmd FileType clojurescript,clojure nnoremap <buffer> <LocalLeader>yd i(def

    " Operate on matching ({[ symbols (VERY helpful)
    " inner first
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> ib %
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> ib( :<C-U>normal! f(vi)<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> ib{ :<C-U>normal! f{vi}<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> ib[ :<C-U>normal! f[vi]<CR>
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> il( :<C-U>normal! F(vi)<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> il{ :<C-U>normal! F{vi}<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> il[ :<C-U>normal! F[vi]<CR>

    " outer second
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> ab %
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> ab( :<C-U>normal! f(va)<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> ab{ :<C-U>normal! f{va}<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> ab[ :<C-U>normal! f[va]<CR>
    autocmd FileType clojurescript,clojure,lisp,scheme onoremap <buffer> al( :<C-U>normal! F(va)<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> al{ :<C-U>normal! F{va}<CR>
    autocmd FileType clojurescript,clojure,scheme onoremap <buffer> al[ :<C-U>normal! F[va]<CR>

    " Crank out those cljs profiles quick!
    " (I should really just write some lein plugins :p)
    autocmd FileType clojurescript,clojure iabbrev leinclojurescript :dependencies [[org.clojure/clojure "1.5.1"]<CR>[org.clojure/clojurescript "0.0-1806"]<CR>[com.cemerick/piggieback "0.0.4"]]<CR><CR>:repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}<CR><CR>:plugins [[lein-cljsbuild "0.3.1"]]<CR><CR>:cljsbuild {<CR>:builds [<CR>{:source-paths ["src"]<CR>:compiler<CR>{:output-to "resources/public/js/out.js"<CR>:optimizations :advanced<CR>:pretty-print :false}}<CR>{:id "dev"<CR>:source-paths ["src"]<CR>:compiler<CR>{:output-to "resources/public/js/out.js"}}]})

augroup END

"}}}

"}}}
" Helpful mappings"{{{

" Easy access to ~/.vimrc
noremap <Leader>ev :tabnew $MYVIMRC<CR>

" Reload vim after editing VIMRC
noremap <Leader>sv :source $MYVIMRC<CR>

" TxtFmt for Butt-Kicking note formatting
nnoremap <Leader>txt :set ft=txtfmt<CR>

" filesystem navigation shortcuts
nnoremap <Leader>ls :!ls<CR>
" nnoremap <Leader>cd :cd<CR>

" number sidebar (oh boy, I'm getting lazy)
nnoremap <Leader>nn :set number!<CR>

" clipboard shortcuts!!!
" nnoremap <Leader>y  :.!pbcopy<CR>
" nnoremap <Leader>p  :r !pbpaste<CR>
noremap <Leader>p "+p<CR>
noremap <Leader>y "+y<CR>

" helpful mappings for tab browsing
nnoremap th  :tabfirst<CR>
nnoremap tj  :tabprev<CR>
nnoremap tk  :tabnext<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>
nnoremap tn  :tabnew<CR>

" Sudo to write
cmap w!! w !sudo tee % >/dev/null

" http://items.sjbach.com/319/configuring-vim-right
" Marks
nnoremap ' `
nnoremap ` '

" Remove the Windows ^M (copied from http://amix.dk/vim/vimrc.html)
map <Leader>dm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Shortcuts for moving between windows
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

noremap <Leader>hl :set hlsearch!<CR>

" http://vim.wikia.com/wiki/Move_cursor_by_display_lines_when_wrapping
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" Setting for writing those git README.md files
noremap <Leader>git :set textwidth=70<CR>

" Toggle spell back on
noremap <Leader>sp :set spell!<CR>

" Sort paragraphs
nnoremap <leader>s vip:!sort<cr>
vnoremap <leader>s :!sort<cr>

" Disable the F1 help key
map <F1> <Esc>
imap <F1> <Esc>

" Snappy jumping in lines (I don't use default H,M,L at all anyway)
noremap H ^
noremap L $
vnoremap L g_

" Toggle Space Folds
nnoremap <Space> za
vnoremap <Space> za

" Undo Tree Toggle
nnoremap <LocalLeader>uu :UndotreeToggle<CR>

" Yank to clipboard register
" (essential if running a repl in another shell)
nnoremap <LocalLeader>y "*y

" Jump to old file
nnoremap <Leader>ol :tabnew<CR>:browse oldfiles<CR>

" swap s for %
noremap s %
noremap % s

"}}}
" Plugins"{{{

" neocomplcache"{{{

noremap <Leader>ne :NeoComplCacheEnable<CR>
noremap <Leader>nd :NeoComplCacheDisable<CR>
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1

"}}}
" ctrl-p"{{{

" highlight colors for omni-complete's Pmenu
highlight   Pmenu         ctermfg=0 ctermbg=6
highlight   PmenuSel      ctermfg=0 ctermbg=7
highlight   PmenuSbar     ctermfg=7 ctermbg=0
highlight   PmenuThumb    ctermfg=0 ctermbg=7

"}}}
" nerdTree"{{{

" Toggle for NERD Tree
noremap <C-n> :NERDTreeToggle<CR>

"}}}
" rainbow-parenthesis {{{

" rainbow parens, nice for clj
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

let g:rbpt_max = 16

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
"}}}
"vim-tmuxify {{{

" These are tmux pannel details
let g:tmuxify_pane_split = '-h'
let g:tmuxify_pane_size = '50'

" Key to shoot blocks into the repl (other panel)
nmap <Leader>ma vip,ms

" Dump the whole file
nmap <Leader>mf ggVG,ms

" Send snippets
nmap <Leader>mx vab,ms

" }}}
"}}}

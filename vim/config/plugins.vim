" PLUGINS

"////////////////////////////////////////////////////////
set rtp+=~/.vim/bundle/vundle
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-vinegar'
Plugin 'git://git.wincent.com/command-t.git'
Plugin 'leafgarland/typescript-vim'
Plugin 'junegunn/limelight.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'scrooloose/nerdtree'
Plugin 'valloric/youcompleteme'
Plugin 'dag/vim-fish'
Plugin 'rhysd/vim-clang-format'
Plugin 'eraserhd/parinfer-rust'
Plugin 'fatih/vim-go'
Plugin 'nickaroot/vim-xcode-dark-theme'
Plugin 'l04m33/vlime', {'rtp': 'vim/'}
call vundle#end()            " required
filetype plugin indent on    " required
"////////////////////////////////////////////////////////



"////////////////////////////////////////////////////////
let g:ctrlp_custom_ignore = 'vendor\|build\|node_modules\|DS_Store\|git\/'
let g:ctrlp_max_depth=40
let g:ctrlp_working_path_mode=''
"////////////////////////////////////////////////////////



"////////////////////////////////////////////////////////
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
"////////////////////////////////////////////////////////




"////////////////////////////////////////////////////////
let g:ycm_global_ycm_extra_conf = '~/dotfiles/vim/.ycm_extra_conf.py'
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_error_symbol = '!!'
let g:ycm_warning_symbol = '??'
"////////////////////////////////////////////////////////




"////////////////////////////////////////////////////////
let g:tmux_navigator_no_mappings = 1 " disable builtin mappings, I think
nnoremap <silent> <M-Left> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-Down> :TmuxNavigateDown<cr>
nnoremap <silent> <M-Up> :TmuxNavigateUp<cr>
nnoremap <silent> <M-Right> :TmuxNavigateRight<cr>
"////////////////////////////////////////////////////////



"////////////////////////////////////////////////////////
let g:go_fmt_command = "goimports"
let g:go_term_mode = "split"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
"////////////////////////////////////////////////////////


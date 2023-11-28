# Remove the greeting from the fish prompt on first open
set fish_greeting ""
export TERM="xterm-256color"

set -e LD_LIBRARY_PATH
set -e LD_INCLUDE_PATH

# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"


switch (uname)
# Specific things to my mac
	case Darwin
		export C_INCLUDE_PATH="/opt/homebrew/include:$C_INCLUDE_PATH"
		export CFLAGS="-I/opt/homebrew/include"
		export LDFLAGS="-L/opt/homebrew/lib"
		export PATH="/opt/homebrew/bin:$PATH"
		export PATH="/Applications/Racket v8.9/bin:$PATH"
end


set -x GOPATH $HOME/dev/go

export PATH="$GOPATH/bin:$PATH"
export PATH="/opt/cuda/bin:$PATH"



export PATH="/usr/local/cuda/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/cuda/lib:$LD_LIBRARY_PATH"

set -g fish_user_paths $HOME/.bin $HOME/.local/bin $HOME/.cargo/bin $fish_user_paths

export EDITOR=(which nvim)
export RVTC="~/chariot/toolchain/local/bin/riscv64-elf-"

alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"

alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias glog "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# alias lg "lazygit"
alias lg "nvim -c :Neogit"
alias lx "exa -lgFBS"

alias gp "git push"
alias gP "git pull"
alias gc "git commit"
alias gcm "git commit -m"
alias gs "git status"
alias gsw "git switch"

# cd to the git root
alias gd "cd (git rev-parse --show-toplevel)"
alias ea "bass source ~/dev/alaska/enable"

alias src "bass source"

alias magit "emacs -e 'magit-status' -e 'delete-other-windows'"

# disable homebrew auto update. Not sure if this works
set -x HOMEBREW_NO_AUTO_UPDATE 1
set HOMEBREW_NO_AUTO_UPDATE 1
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish


set -g hydro_color_pwd yellow
set -g hydro_color_prompt green
set -g hydro_color_error red

set -g fish_color_normal normal
set -g fish_color_command ffffff
set -g fish_color_quote a8a8a8
set -g fish_color_redirection 808080
set -g fish_color_end 949494
set -g fish_color_error 585858
set -g fish_color_param d7d7d7
set -g fish_color_comment bcbcbc
set -g fish_color_match --background=brblue
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_history_current --bold
set -g fish_color_operator 00a6b2
set -g fish_color_escape 00a6b2
set -g fish_color_cwd green
set -g fish_color_cwd_root red
set -g fish_color_valid_path --underline
set -g fish_color_autosuggestion 777777
set -g fish_color_user brgreen
set -g fish_color_host normal
set -g fish_color_cancel -r
set -g fish_pager_color_completion normal
set -g fish_pager_color_description B3A06D yellow
set -g fish_pager_color_prefix white --bold --underline
set -g fish_pager_color_progress brwhite --background=cyan

set -gx PNPM_HOME "/Users/nick/Library/pnpm"
set -gx PATH "$PNPM_HOME" $PATH

# opam configuration
source /Users/nick/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
source ~/dotfiles/fish/iterm.fish; or true


alias enable_conda 'eval $HOME/anaconda3/bin/conda "shell.fish" "hook" $argv | source'
#>>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if [ -d $HOME/anaconda3 ]
	# eval $HOME/anaconda3/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

# set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ;
set -gx PATH "$HOME/.cabal/bin" "$HOME/.ghcup/bin" $PATH # ghcup-env
set -gx PATH "/nix/var/nix/profiles/default/bin" $PATH # ghcup-env

if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish' ]
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
end

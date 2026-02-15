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

export COLORTERM="truecolor"

export PATH="$GOPATH/bin:$PATH"
export PATH="/opt/cuda/bin:$PATH"
export PATH="/tank/nick/local/bin:$PATH"



export PATH="/usr/local/cuda/bin:$PATH"
# export LD_LIBRARY_PATH="/usr/local/cuda/lib:$LD_LIBRARY_PATH"

set -g fish_user_paths $HOME/.bin $HOME/.local/bin $HOME/.cargo/bin $fish_user_paths

export EDITOR=(which nvim)
export RVTC="~/chariot/toolchain/local/bin/riscv64-elf-"

alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"

alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias ndev="NDEV_DIR=(pwd) nix develop --command fish"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias glog "git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all"

# alias lg "lazygit"
alias lg "nvim -c :Neogit"
alias lx "exa -lgFBS"

# cd to the git root
alias src "bass source"

alias magit "nvim -c :Neogit"

# Run emacs in a client
alias em "emacsclient --create-frame --alternate-editor=''"

# disable homebrew auto update. Not sure if this works
set -x HOMEBREW_NO_AUTO_UPDATE 1
set HOMEBREW_NO_AUTO_UPDATE 1
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish



set -g fish_color_normal normal
set -g fish_color_command white
set -g fish_color_quote yellow
set -g fish_color_redirection --underline brgreen
set -g fish_color_end green
set -g fish_color_error red
set -g fish_color_param white
set -g fish_color_comment brblack
set -g fish_color_match --background=brblue
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_history_current --bold
set -g fish_color_operator red
set -g fish_color_escape brgreen
set -g fish_color_valid_path --bold --underline brblue
set -g fish_color_autosuggestion brblack
set -g fish_color_cancel -r

set -gx PNPM_HOME "/Users/nick/Library/pnpm"
set -gx PATH "$PNPM_HOME" $PATH

source /Users/nick/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true



set -gx PATH "$HOME/.nix-profile/bin" $PATH

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


if test -e ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  bass source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
end

if type -q direnv
  direnv hook fish | source
end

# Amp CLI
export PATH="/Users/nick/.amp/bin:$PATH"

# opencode
fish_add_path /Users/nick/.opencode/bin

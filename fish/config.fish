# Remove the greeting from the fish prompt on first open
set fish_greeting ""
set HOMEBREW_NO_AUTO_UPDATE 1
export TERM="xterm-256color"
set -x GOPATH $HOME/dev/go

mkdir -p ~/.local/bin
export PATH="$HOME/.local/bin:$PATH"

switch (uname)
# Specific things to my mac
case Darwin
	export PATH="$PATH:/usr/local/bin:/usr/local/sbin"
	# export PATH="$PATH:/usr/local/opt/python@2/libexec/bin"

	# latex crap
	# export PATH="/usr/local/texlive/2019/bin/x86_64-darwin:$PATH"
	export PATH="$PATH:$HOME/.bin"
	export PATH="$PATH:$HOME/.cargo/bin:/opt/local/bin"
	# export PATH="$PATH:$GOPATH/bin"
end

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"


# Setup ~/.bin as a PATH location.
mkdir -p ~/.bin
export PATH="$HOME/.bin:$PATH"

export EDITOR=(which nvim)

alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"

alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias glog "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

alias lg "lazygit"
alias lx "exa -lgaFBS"


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc' ];
	if type source > /dev/null;
		source '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc';
	else;
		. '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc';
	end;
end

# disable homebrew auto update. Not sure if this works
set -x HOMEBREW_NO_AUTO_UPDATE 1
set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/binutils/bin" $fish_user_paths
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

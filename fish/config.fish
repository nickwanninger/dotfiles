# Remove the greeting from the fish prompt on first open
set fish_greeting ""
set HOMEBREW_NO_AUTO_UPDATE 1
export TERM="xterm-256color"
# Show a pretty calendar :)
# cal
#
set -x GOPATH $HOME/dev/go


mkdir -p ~/.local/bin
export PATH="$HOME/.local/bin:$PATH"


switch (uname)
# Specific things to my mac
case Darwin
  # export PATH="/usr/local/opt/python/libexec/bin:$PATH"
	export PATH="$HOME/.bin:$PATH"
	export PATH="$HOME/.cargo/bin:/opt/local/bin:$PATH"
	export PATH="$GOPATH/bin:$PATH"
end


export EDITOR=(which nvim)


alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"



alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias kb "keybase"
alias aplay "mplayer -cache 1024 -quiet -rawaudio samplesize=1:channels=1:rate=8000 -demuxer rawaudio -"
alias remake "make clean; make"

alias glog "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"


alias mk "make"
alias md "make debug"


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc' ]; if type source > /dev/null; source '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc'; else; . '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc'; end; end

# disable homebrew auto update. Not sure if this works
set -x HOMEBREW_NO_AUTO_UPDATE 1
set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths

set -g fish_user_paths "/usr/local/opt/binutils/bin" $fish_user_paths

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

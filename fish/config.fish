# Remove the greeting from the fish prompt on first open
set fish_greeting ""
set HOMEBREW_NO_AUTO_UPDATE 1
export TERM="xterm-256color"
# Show a pretty calendar :)
# cal
switch (uname)
case Darwin
  # export PATH="/usr/local/opt/python/libexec/bin:$PATH"
end


export PATH="$HOME/.cargo/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export EDITOR=(which vim)
set -x GOPATH $HOME/dev/go/

alias t "tmux"
alias v "vim"
alias :q "exit"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc' ]; if type source > /dev/null; source '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc'; else; . '/Users/nick/.gcloud/google-cloud-sdk/path.fish.inc'; end; end

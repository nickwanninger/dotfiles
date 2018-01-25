# Remove the greeting from the fish prompt on first open
set fish_greeting ""
set HOMEBREW_NO_AUTO_UPDATE 1
export TERM="xterm-256color"

switch (uname)
case Darwin
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
end
export PATH="$HOME/.cargo/bin:$PATH"
export EDITOR=(which vim)
set -x GOPATH $HOME/dev/go/

alias t "tmux"

alias :q "exit"

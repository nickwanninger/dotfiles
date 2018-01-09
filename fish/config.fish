# Remove the greeting from the fish prompt on first open
set fish_greeting ""
set HOMEBREW_NO_AUTO_UPDATE 1

switch (uname)
case Darwin
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
end


set -x GOPATH $HOME/dev/go/

alias t "tmux"

alias :q "exit"

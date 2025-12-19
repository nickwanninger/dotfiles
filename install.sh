#!/usr/bin/env bash
DOTFILES="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Ensure that some dirs exist
mkdir -p ~/.config
mkdir -p ~/.local/{bin,local,lib}

# echo "Installing fish configuration..."
# rm -rf ~/.config/fish
# ln -sF ${DOTFILES}/fish/ ~/.config


function link_dotconfig() {
  name=$1
  echo "Installing ${name} to ~/.config/${name}"
  rm -rf ~/.config/${name}
  ln -sF ${DOTFILES}/${name} ~/.config/${name}
}



link_dotconfig "fish"
link_dotconfig "nix"
link_dotconfig "ghostty"
link_dotconfig "home-manager"
link_dotconfig "nvim"


# Setup emacs
rm -rf ~/.emacs.d
ln -sF ${DOTFILES}/emacs ~/.emacs.d

# TMUX is special
ln -sf ${DOTFILES}/tmux/tmux.conf ~/.tmux.conf

echo "Configuring NeoVIM and installing bundles..."

echo "installing terminfos"
tic -x misc/xterm-256color-italic.terminfo
tic -x misc/tmux-256color.terminfo

echo "Setup Complete!"



# Compile emacs's parinfer binary for this arch
# -- This is disabled for now --
# mkdir -p ~/dev
# pushd ~/dev
#   rm -rf parinfer-rust-emacs
#   git clone https://github.com/justinbarclay/parinfer-rust-emacs.git
#   cd parinfer-rust-emacs
#   cargo build --release
#   mkdir -p ~/.cache/emacs/parinfer-rust
#   cp ./target/release/libparinfer_rust* ~/.cache/emacs/parinfer-rust
# 
# 
#   mkdir -p ~/.emacs.d/parinfer-rust/
#   cp ./target/release/libparinfer_rust* ~/.emacs.d/parinfer-rust/
# popd

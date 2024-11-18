#!/usr/bin/env bash
set -ex

DOTFILES="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Ensure that some dirs exist
mkdir -p ~/.config
mkdir -p ~/.local/{bin,local,lib}

echo "Installing fish configuration..."
rm -rf ~/.config/fish
ln -sF ${DOTFILES}/fish/ ~/.config


echo "Installing nix configuration..."
rm -rf ~/.config/nix
ln -sF ${DOTFILES}/nix/ ~/.config

rm -rf ~/.config/home-manager
ln -sF ${DOTFILES}/home-manager/ ~/.config

# Setup emacs
rm -rf ~/.emacs.d
ln -sF ${DOTFILES}/emacs ~/.emacs.d

echo "Installing tmux config"
ln -sf ${DOTFILES}/tmux/tmux.conf ~/.tmux.conf

echo "Configuring NeoVIM and installing bundles..."

rm -f ~/.config/nvim
ln -sF ${DOTFILES}/nvim ~/.config/nvim
# nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

echo "installing terminfos"
tic -x misc/xterm-256color-italic.terminfo
tic -x misc/tmux-256color.terminfo

echo "Setup Complete!"



# Compile emacs's parinfer binary for this arch

mkdir -p ~/dev
pushd ~/dev
  rm -rf parinfer-rust-emacs
  git clone https://github.com/justinbarclay/parinfer-rust-emacs.git
  cd parinfer-rust-emacs
  cargo build --release
  mkdir -p ~/.cache/emacs/parinfer-rust
  cp ./target/release/libparinfer_rust* ~/.cache/emacs/parinfer-rust


  mkdir -p ~/.emacs.d/parinfer-rust/
  cp ./target/release/libparinfer_rust* ~/.emacs.d/parinfer-rust/
popd

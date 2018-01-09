#!/usr/bin/env bash

ln -sF ~/dotfiles/fish/ ~/.config

ln -sf ~/dotfiles/vim/vimrc ~/.vimrc
ln -sF ~/dotfiles/vim/ ~/.vim

ln -sf ~/dotfiles/git/.gitignore ~/.gitignore

ln -sf ~/dotfiles/tmux/tmux.conf ~/.tmux.conf

# defaults write com.apple.dock orientation -string left
# defaults write com.apple.dock autohide-time-modifier -float 0.12
# defaults write com.apple.dock autohide-delay -float 0
# killall Dock

BUNDLE_DIR=~/.vim/bundle

# Install/update Vundle
mkdir -p "$BUNDLE_DIR" && (git clone https://github.com/gmarik/vundle.git "$BUNDLE_DIR/vundle" || (cd "$BUNDLE_DIR/vundle" && git pull origin master))

# Install bundles
vim +PluginInstall +qall

echo "Setup Complete!"

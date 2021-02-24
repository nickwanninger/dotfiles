#!/usr/bin/env bash
rm -rf ~/.config/fish
ln -sF ~/dotfiles/fish/ ~/.config


rm -rf ~/.config/alacritty
ln -sF ~/dotfiles/alacritty/ ~/.config

rm -rf ~/.vim
ln -sf ~/dotfiles/vim/vimrc ~/.vimrc
ln -sF ~/dotfiles/vim/ ~/.vim

ln -s ../.vim ~/.config/nvim
ln -s ../.vimrc ~/.vim/init.vim

ln -sf ~/dotfiles/git/.gitignore ~/.gitignore

ln -sf ~/dotfiles/tmux/tmux.conf ~/.tmux.conf

# defaults write com.apple.dock orientation -string left
# defaults write com.apple.dock autohide-time-modifier -float 0.12
# defaults write com.apple.dock autohide-delay -float 0
# killall Dock


echo "Setting up vim-plug"

BUNDLE_DIR=~/.vim/autoload

rm -rf "$BUNDLE_DIR/*"
mkdir -p "$BUNDLE_DIR"

curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install bundles
vim +PlugInstall +UpdateRemotePlugins +qa

echo "installing terminfos"
tic -x xterm-256color-italic.terminfo
tic -x tmux-256color.terminfo

echo "Setup Complete!"

#!/usr/bin/env sh

ln -sF ~/dotfiles/fish/ ~/.config


ln -sF ~/dotfiles/vim/vimrc ~/.nvimrc
ln -sF ~/dotfiles/vim/vimrc ~/.vimrc
ln -sF ~/dotfiles/vim/ ~/.vim
ln -sF ~/dotfiles/vim/ ~/.config/nvim

ln -sF ~/dotfiles/git/.gitignore ~/.gitignore

ln -sF ~/dotfiles/tmux/tmux.conf ~/.tmux.conf

defaults write com.apple.dock orientation -string left
defaults write com.apple.dock autohide-time-modifier -float 0.12
defaults write com.apple.dock autohide-delay -float 0
killall Dock
echo "Setup Complete!"

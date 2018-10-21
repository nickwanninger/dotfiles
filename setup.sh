#!/usr/bin/env bash
rm -rf ~/.config/fish
ln -sF ~/dotfiles/fish/ ~/.config

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



echo "Setup Complete!"

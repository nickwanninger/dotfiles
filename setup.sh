#! /usr/bin/env sh

rm -rf ~/.config/fish
ln -s ~/dotfiles/fish/ ~/.config


rm -rf ~/.vimrc
ln -s ~/dotfiles/vim/vimrc ~/.vimrc

echo "Setup Complete!"

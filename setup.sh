#! /usr/bin/env sh

rm -rf ~/.config/fish
ln -s ~/dotfiles/fish/ ~/.config


rm -rf ~/.vimrc
rm -rf ~/.vim

ln -s ~/dotfiles/vim/vimrc ~/.vimrc
ln -s ~/dotfiles/vim/ ~/.vim

rm -rf ~/.gitignore
ln -s ~/dotfiles/git/.gitignore ~/.gitignore


echo "Setup Complete!"

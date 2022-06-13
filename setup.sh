#!/usr/bin/env bash
set -x
set -e
mkdir -p ~/.config

# Make sure that ~/.local/ is a valid prefix
mkdir -p ~/.local/{bin,local,lib}

echo "setting up fish config"
rm -rf ~/.config/fish
ln -sF ~/dotfiles/fish/ ~/.config


echo "setting up alacritty config"
rm -rf ~/.config/alacritty
ln -sF ~/dotfiles/alacritty/ ~/.config

echo "setting up vim config"
rm -rf ~/.vim
ln -sf ~/dotfiles/vim/vimrc ~/.vimrc
ln -sF ~/dotfiles/vim/ ~/.vim


rm -rf ~/.config/nvim
ln -s ../.vim ~/.config/nvim
# ln -s ../.vimrc ~/.vim/init.vim
mkdir -p ~/.config/nvim
# ln -s ../.vimrc ~/.config/nvim/init.vim

ln -sf ~/dotfiles/git/.gitignore ~/.gitignore

ln -sf ~/dotfiles/tmux/tmux.conf ~/.tmux.conf

echo "Setting up vim-plug"

BUNDLE_DIR=~/.vim/autoload

rm -rf "$BUNDLE_DIR/*"
mkdir -p "$BUNDLE_DIR"

curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install bundles
nvim +PlugInstall +UpdateRemotePlugins +qa

echo "installing terminfos"
tic -x xterm-256color-italic.terminfo
tic -x tmux-256color.terminfo

echo "Setup Complete!"

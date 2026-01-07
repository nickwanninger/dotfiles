#!/usr/bin/env bash
DOTFILES="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Ensure that some dirs exist
mkdir -p ~/.config
mkdir -p ~/.local/{bin,local,lib}

# echo "Installing fish configuration..."
# rm -rf ~/.config/fish
# ln -sF ${DOTFILES}/fish/ ~/.config


link_dotconfig() {
  local source_name=$1
  local target_path=${2:-"$HOME/.config/$source_name"}
  local source_path="${DOTFILES}/${source_name}"

  # Check if source exists in dotfiles
  if [[ ! -e "$source_path" ]]; then
    echo "Error: Source does not exist: $source_path"
    return 1
  fi

  # If target already exists
  if [[ -e "$target_path" ]]; then
    # If it's a symlink, check if it points to our dotfiles
    if [[ -L "$target_path" ]]; then
      local current_link=$(readlink "$target_path")
      if [[ "$current_link" == "$source_path" ]]; then
        echo "✓ ${source_name} Already linked correctly"
        return 0
      else
        echo "! ${source_name} Symlink points to different location: $current_link"
      fi
    else
      # It's a real directory/file, ask user
      echo "! Target for ${source_name} already exists (not a symlink): $target_path"
      read -p "  Replace it? (y/n) " -n 1 -r
      echo
      if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "  Skipping ${source_name}"
        return 0
      fi
    fi

    # Remove the old target
    rm -rf "$target_path"
  fi

  # Create the symlink
  ln -sF "$source_path" "$target_path"
  echo "  ✓ Linked"
}



link_dotconfig "fish"
link_dotconfig "ghostty"
link_dotconfig "nvim"
link_dotconfig "emacs" "$HOME/.emacs.d"

# TMUX is special - link the config file directly
link_dotconfig "tmux/tmux.conf" "$HOME/.tmux.conf"



link_dotconfig "nix"
link_dotconfig "home-manager"

if command -v nix >/dev/null 2>&1; then
  echo "Updating home manager"
  make -C ${DOTFILES}/home-manager
fi


tic -x "${DOTFILES}/misc/xterm-256color-italic.terminfo"
tic -x "${DOTFILES}/misc/tmux-256color.terminfo"


link_dotconfig ai/claude/skills ~/.claude/skills
link_dotconfig ai/claude/CLAUDE.md ~/.claude/CLAUDE.md

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

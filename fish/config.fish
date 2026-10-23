# Remove the greeting from the fish prompt on first open
set fish_greeting ""
export TERM="xterm-256color"

set -e LD_LIBRARY_PATH
set -e LD_INCLUDE_PATH

# Keep platform paths separate from user tools.
set -l platform_paths
switch (uname)
    case Darwin
        # Remove Linux paths inherited from an older shell.
        set -l linux_paths '^(/home/|/tank/|/opt/cuda/|/usr/local/cuda/)'
        if set -q fish_user_paths
            set -g fish_user_paths (string match -rv $linux_paths -- $fish_user_paths)
        end
        set -gx PATH (string match -rv $linux_paths -- $PATH)

        if not contains -- /opt/homebrew/include $C_INCLUDE_PATH
            set -gx C_INCLUDE_PATH /opt/homebrew/include $C_INCLUDE_PATH
        end
        set -gx CFLAGS -I/opt/homebrew/include
        set -gx LDFLAGS -L/opt/homebrew/lib
        set -gx HOMEBREW_NO_AUTO_UPDATE 1
        set -gx PNPM_HOME "$HOME/Library/pnpm"
        set platform_paths "/Applications/Racket v8.9/bin" /opt/homebrew/bin
    case Linux
        set -gx PNPM_HOME "$HOME/.local/share/pnpm"
        set platform_paths /usr/local/cuda/bin /tank/nick/local/bin /opt/cuda/bin
end

set -gx GOPATH "$HOME/dev/go"
set -gx BUN_INSTALL "$HOME/.bun"
set -gx COLORTERM truecolor

# Add existing directories without writing universal variables.
fish_add_path --path --move \
    "$BUN_INSTALL/bin" \
    "$HOME/.opencode/bin" \
    "$HOME/.bin" \
    "$HOME/.local/bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.nix-profile/bin" \
    /nix/var/nix/profiles/default/bin \
    "$HOME/.amp/bin" \
    "$HOME/.cabal/bin" \
    "$HOME/.ghcup/bin" \
    "$HOME/.npm-global/bin" \
    "$PNPM_HOME" \
    "$GOPATH/bin" \
    $platform_paths
fish_add_path --path --append "$HOME/.lmstudio/bin"

set -gx EDITOR nvim
set -gx RVTC "$HOME/chariot/toolchain/local/bin/riscv64-elf-"

alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"

alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias ndev="NDEV_DIR=(pwd) nix develop --command fish"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias glog "git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all"

# alias lg "lazygit"
alias lg "nvim -c :Neogit"
alias lx "exa -lgFBS"

# cd to the git root
alias src "bass source"

alias magit "nvim -c :Neogit"

# Run emacs in a client
alias em "emacsclient --create-frame --alternate-editor=''"

if status is-interactive; and test -r "$HOME/.iterm2_shell_integration.fish"
    source "$HOME/.iterm2_shell_integration.fish"
end



set -g fish_color_normal normal
set -g fish_color_command white
set -g fish_color_quote yellow
set -g fish_color_redirection --underline brgreen
set -g fish_color_end green
set -g fish_color_error red
set -g fish_color_param white
set -g fish_color_comment brblack
set -g fish_color_match --background=brblue
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_history_current --bold
set -g fish_color_operator red
set -g fish_color_escape brgreen
set -g fish_color_valid_path --bold --underline brblue
set -g fish_color_autosuggestion brblack
set -g fish_color_cancel -r

if test -r "$HOME/.opam/opam-init/init.fish"
    source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>&1
end

alias enable_conda 'eval $HOME/anaconda3/bin/conda "shell.fish" "hook" $argv | source'
# conf.d/nix-env.fish initializes Nix before this file.
if not set -q __HM_SESS_VARS_SOURCED; and test -r "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    bass source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
end

# Nix can already install the direnv hook through vendor_conf.d.
if status is-interactive; and not functions -q __direnv_export_eval; and command -q direnv
    direnv hook fish | source
end

# Keep the first occurrence so nested shells do not grow PATH.
set -l unique_paths
for path_entry in $PATH
    if not contains -- $path_entry $unique_paths
        set -a unique_paths $path_entry
    end
end
set -gx PATH $unique_paths

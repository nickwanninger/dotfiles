# Remove the greeting from the fish prompt on first open
#
set fish_greeting ""
export TERM="xterm-256color"
set -x GOPATH $HOME/dev/go


mkdir -p ~/.local/bin
# export PATH="$HOME/.local/bin:$PATH"


switch (uname)
# Specific things to my mac
case Darwin
	# export PATH="$PATH:/usr/local/bin:/usr/local/sbin"
end

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"


# Setup ~/.bin as a PATH location.
mkdir -p ~/.bin

# export PATH="$HOME/.bin:$PATH"
# export PATH="$HOME/.cargo/bin:$PATH"

set -g fish_user_paths $HOME/.bin $HOME/.local/bin $HOME/.cargo/bin $fish_user_paths

export EDITOR=(which nvim)

alias cft "vim ~/dotfiles/tmux/tmux.conf"
alias cff "vim ~/dotfiles/fish/config.fish"

alias vim="nvim"
alias vi="nvim"
alias oldvim="vim"

alias tm "tmux new-session -A -s main"
alias :q "exit"
alias glog "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

alias lg "lazygit"
alias lx "exa -lgaFBS"

# disable homebrew auto update. Not sure if this works
set -x HOMEBREW_NO_AUTO_UPDATE 1
set HOMEBREW_NO_AUTO_UPDATE 1
test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish



set -g hydro_color_pwd yellow
set -g hydro_color_prompt green
set -g hydro_color_error red


set -g fish_color_normal normal
set -g fish_color_command ffffff
set -g fish_color_quote a8a8a8
set -g fish_color_redirection 808080
set -g fish_color_end 949494
set -g fish_color_error 585858
set -g fish_color_param d7d7d7
set -g fish_color_comment bcbcbc
set -g fish_color_match --background=brblue
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_history_current --bold
set -g fish_color_operator 00a6b2
set -g fish_color_escape 00a6b2
set -g fish_color_cwd green
set -g fish_color_cwd_root red
set -g fish_color_valid_path --underline
set -g fish_color_autosuggestion 777777
set -g fish_color_user brgreen
set -g fish_color_host normal
set -g fish_color_cancel -r
set -g fish_pager_color_completion normal
set -g fish_pager_color_description B3A06D yellow
set -g fish_pager_color_prefix white --bold --underline
set -g fish_pager_color_progress brwhite --background=cyan

{ pkgs, username, home, stateVersion, ... }:

{
  home.username = username;
  home.homeDirectory = home;
  home.stateVersion = stateVersion;

  home.packages = [
    pkgs.neovim
    pkgs.direnv
    pkgs.emacs
  ];

  home.file = {};

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

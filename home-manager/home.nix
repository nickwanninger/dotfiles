{ pkgs, config, ... }:

{
  home.username = config.username;
  home.homeDirectory = config.home;
  home.stateVersion = config.stateVersion;

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

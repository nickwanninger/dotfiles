{ pkgs, config, ... }:

{
  home.username = config.username;
  home.homeDirectory = config.home;
  home.stateVersion = config.stateVersion;

  home.packages = [

    pkgs.neovim
    pkgs.direnv
    pkgs.emacs-nox
    pkgs.guile
    pkgs.meson
    pkgs.gmime3
    pkgs.gmime

    pkgs.nodejs
    pkgs.git

    # The uv python package manager
    pkgs.uv

    pkgs.mosh

    pkgs.texliveFull

    # email stuff for mu4e
    pkgs.isync
    pkgs.mu
    pkgs.mu.mu4e
    pkgs.msmtp


    # pkgs.racket # The racket language
  ];

  home.file = {};

  home.sessionVariables = {
    EDITOR = "nvim";
  };



  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

{ pkgs, config, ... }:

{
  home.username = config.username;
  home.homeDirectory = config.home;
  home.stateVersion = "25.05";

  # NOT sure about this!
  # home.enableNixpkgsReleaseCheck = false;

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


    # AI tools to steal my job.
    pkgs.claude-code

    pkgs.zig

    # pkgs.racket # The racket language
  ] ++ (if config.system != "aarch64-darwin" then
  [
    # Linux specific packages
    pkgs.poop # :)

  ] else []);

  home.file = {};

  home.sessionVariables = {
    EDITOR = "nvim";
  };



  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

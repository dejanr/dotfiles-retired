{ config, lib, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;

  environment.systemPackages = [
    pkgs.nix-repl
    pkgs.tmux
    pkgs.vim
    pkgs.gitAndTools.gitFull
    pkgs.gitAndTools.git-extras
    pkgs.gitAndTools.gitflow
    pkgs.nodejs-8_x
    pkgs.imagemagick
    pkgs.fastlane
    pkgs.jdk
    pkgs.ag
    pkgs.minecraft
    pkgs.minecraft-server
  ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 2;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 4;
}

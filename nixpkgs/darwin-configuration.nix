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
    pkgs.ack
    pkgs.minecraft
    pkgs.minecraft-server
    pkgs.reattach-to-user-namespace
  ];

  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 3;
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "left";
  system.defaults.dock.showhidden = false;
  system.defaults.dock.mru-spaces = false;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  system.defaults.trackpad.Clicking = true;
  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  programs.bash.enable = true;
  programs.nix-index.enable = true;

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 2;

  nix.maxJobs = 4;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';
  nix.nixPath = [
    # Use local nixpkgs checkout instead of channels.
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "darwin=$HOME/.nix-defexpr/darwin"
    "nixpkgs=$HOME/.nix-defexpr/nixpkgs"
    "$HOME/.nix-defexpr/channels"
  ];
}

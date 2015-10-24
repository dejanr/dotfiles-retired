{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./configuration/common.nix
      ./configuration/xmonad.nix
    ];

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    supportedFilesystems = [ "zfs" ];
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.devices = [ "/dev/sda" ];
  };

  fileSystems = [
    {
      mountPoint = "/";
      device = "rpool/root/nixos";
      fsType = "zfs";
    }
    {
      mountPoint = "/boot";
      device = "/dev/sda1";
      fsType = "ext3";
    }
  ];

  networking = {
    hostId = "8e27eca5";
    hostName = "workstation";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
  };

  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
    };
  };
}

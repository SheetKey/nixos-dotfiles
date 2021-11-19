# Scripts defaul.nix

{ pkgs, ... }:

let
  getKernelScript = pkgs.writeShellScriptBin "getKernelScript" ''
    #! /bin/bash

    kern="$(uname -r)"
    echo -e "$kern "
  '';

in {
  environment.systemPackages = [ getKernelScript ];
}

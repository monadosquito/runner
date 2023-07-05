let
    pin = import ./chr/pin.nix;
in
    {
        nixpkgs ? import pin.nixpkgs {},
    }
    :
    nixpkgs.haskellPackages.callCabal2nix "runner" ./. {}

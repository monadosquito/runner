let pin = import ./chr/pin.nix;
    config = {
        allowBroken = true;
    };
    overlays = import ./chr/overlays.nix;
in
    {
        nixpkgs ? import pin.nixpkgs {inherit config;},
        miso ? import pin.miso {},
    }
    :
    let hsPkgs = nixpkgs.haskellPackages.extend overlays.brick;
    in
    {
        withGhc = hsPkgs.callCabal2nix "runner" ./. {};
        withGhcjs = miso.pkgs.haskell.packages.ghcjs.callCabal2nix
                        "runner"
                        ./.
                        {};
    }

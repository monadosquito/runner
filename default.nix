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
        ghcRunner = hsPkgs.callCabal2nix "runner" ./. {};
        ghcjsRunner = miso.pkgs.haskell.packages.ghcjs.callCabal2nix
                          "runner"
                          ./.
                          {};
        consRunner = nixpkgs.haskell.lib.enableCabalFlag ghcRunner "cons";
        wwwRunner = nixpkgs.haskell.lib.enableCabalFlag ghcjsRunner "www";
    in
    {
        cons = consRunner;
        www = wwwRunner;
    }

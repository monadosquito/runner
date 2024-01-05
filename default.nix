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
        dryRunConsRunner = nixpkgs.haskell.lib.enableCabalFlag
                               consRunner
                               "dry-run";
        wwwRunner = nixpkgs.haskell.lib.enableCabalFlag ghcjsRunner "www";
        dryRunWwwRunner = nixpkgs.haskell.lib.enableCabalFlag
                              wwwRunner
                              "dry-run";
        inclCss = nixpkgs.lib.readFile ./scr/incl-css.sh;
        wwwRunnerWithCss = wwwRunner.overrideAttrs (prev: {
            installPhase = prev.installPhase + inclCss;
        });
    in
    {
        cons = consRunner;
        dry-run-cons = dryRunConsRunner;
        www = wwwRunnerWithCss;
        dry-run-www = dryRunWwwRunner;
    }

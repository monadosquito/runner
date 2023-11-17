let pin = import ./chr/pin.nix;
    config = {
        allowBroken = true;
    };
    overlays = import ./chr/overlays.nix;
in
    {
        nixpkgs ? import pin.nixpkgs {inherit config;},
    }
    :
    let hsPkgs = nixpkgs.haskellPackages.extend overlays.brick;
        runner = hsPkgs.callCabal2nix "runner" ./. {};
    in
    nixpkgs.mkShell
        {
            buildInputs = [
                hsPkgs.cabal-install
                (nixpkgs.writeShellScriptBin
                     "watch"
                     (nixpkgs.lib.readFile ./scr/watch.sh)
                )
            ];
            inputsFrom = [runner.env];
        }

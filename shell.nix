let
    pin = import ./chr/pin.nix;
in
    {
        nixpkgs ? import pin.nixpkgs {},
    }
    :
    nixpkgs.mkShell
        {
            buildInputs = [
                              nixpkgs.haskellPackages.cabal-install
                              (nixpkgs.writeShellScriptBin
                                   "watch"
                                   (nixpkgs.lib.readFile ./scr/watch.sh)
                              )
                          ];
            inputsFrom = [
                             (nixpkgs.haskellPackages.callCabal2nix
                                  "runner"
                                  ./.
                                  {}
                             ).env
                         ];
        }

{
    brick = self: _: {
        brick = self.callCabal2nix
                    "brick"
                    (fetchTarball
                         https://github.com/jtdaugherty/brick/archive/1.9.tar.gz
                    )
                    {};
        text-zipper = self.callCabal2nix
                          "text-zipper"
                          (fetchTarball
                               https://github.com/jtdaugherty/text-zipper/archive/0.13.tar.gz
                          )
                          {};
        vty = self.callCabal2nix
                  "vty"
                  (fetchTarball
                       https://github.com/jtdaugherty/vty/archive/5.39.tar.gz
                  )
                  {};
        bimap = self.callHackageDirect
                    {
                        pkg = "bimap";
                        ver = "0.5.0";
                        sha256 = "1p1bqvkbzjkwhrhhwcx0d4j52pa7287jdh45c8xzgksh1z33xg55";
                    }
                    {};
    };
}

let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz";
  }) { };

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "5926981701ac781f08b02e31e4705e46b799299d";
    sha256 = "03g9xq451dmrkq8kiz989wnl8k0lmj60ajflz44bhp7cm08hf3bw";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-store";
  buildInputs = with pursPkgs; [
    pursPkgs.purs-0_15_4
    pursPkgs.spago
    pursPkgs.psa
    pursPkgs.purs-tidy
    pursPkgs.purescript-language-server

    pkgs.nodejs-16_x
    pkgs.esbuild
  ];
}

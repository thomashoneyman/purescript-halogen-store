let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "721bbd957c62594c46ea4c94f1a9f3cb341b2d25";
    sha256 = "1n72h9y4yfqkk38a2bms1y8qvy6bkdsj27vicf7f9xl92kpzbyab";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-store";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.psa
    pursPkgs.purs-tidy

    pkgs.nodejs-16_x
  ];
}

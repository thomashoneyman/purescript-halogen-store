let
  # Pinned nixos-25.11. npm ci provides the same tools as CI, including editor tools.
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b6018f87da91d19d0ab4cf979885689b469cdd41.tar.gz";
  }) { };
in pkgs.mkShell {
  name = "halogen-store";
  packages = [
    pkgs.nodejs_24
    pkgs.git
  ];
  shellHook = ''
    export PATH="$PWD/node_modules/.bin:$PATH"
  '';
}

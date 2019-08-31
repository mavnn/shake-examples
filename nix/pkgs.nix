let
  fetch = { rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = sha256;
    };
in
rec {
  pkgsPath = fetch {
    rev = "9ec7625cee5365c741dee7b45a19aff5d5d56205";
    sha256 = "0rh26fhdvnp9ssk8g63ysyzigw9zg43k9bd2fzrvhrk75sav723h";
  };
  pkgs = import pkgsPath { config = {}; };
}
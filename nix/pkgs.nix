let
  fetch = { rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = sha256;
    };
in
rec {
  pkgsPath = fetch {
    rev = "6a3f5bcb061e1822f50e299f5616a0731636e4e7";
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };
  pkgs = import pkgsPath { config = {}; };
}
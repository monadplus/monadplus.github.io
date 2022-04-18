with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "monadplus.github.io";
  buildInputs = [
    ruby.devEnv
    pkg-config
    bundix
    gnumake
  ];
}

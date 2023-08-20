with import <nixpkgs>
{
  config = {
    permittedInsecurePackages = [
      "ruby-2.7.8"
      "openssl-1.1.1u"
    ];
  };
};
stdenv.mkDerivation {
  name = "monadplus.github.io";
  buildInputs = [
    ruby_2_7.devEnv
    pkg-config
    bundix
    gnumake
  ];
}

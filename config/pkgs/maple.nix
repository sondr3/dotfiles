{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig }:

rustPlatform.buildRustPackage rec {
  pname = "maple";
  version = "0.5";

  src = fetchFromGitHub {
    owner = "liuchengxu";
    repo = "vim-clap";
    rev = "f74287222cbc65edcc5623ac2de61d62c624f533";
    sha256 = "03l19vlxhl7r1q4sx6lakrflc095zsjpndjdgj452n0ngcgywp3q";
  };

  cargoSha256 = "1zkkg87mjwv42hyvzfn70c51j7lmq6qmpp7a0sz5c7g89x39ih3s";

  nativeBuildInputs = [ pkgconfig ];

  meta = with stdenv.lib; {
    description = "Modern performant generic finder and dispatcher for Vim and NeoVim";
    homepage = https://github.com/liuchengxu/vim-clap;
    license = with licenses; [ mit ];
  };
}

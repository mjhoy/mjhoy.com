{ stdenv }:

stdenv.mkDerivation {
  name = "libfoo-0.1";

  src = ./.;

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/include

    cp libfoo.a $out/lib
    cp foo.h    $out/include    
  '';
}

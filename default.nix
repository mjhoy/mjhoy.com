{ mkDerivation, base, hakyll, pandoc, stdenv }:
mkDerivation {
  pname = "blog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc ];
  homepage = "http://mjhoy.com";
  description = "mjhoy.com website";
  license = stdenv.lib.licenses.mit;
}

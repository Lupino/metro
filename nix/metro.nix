{ mkDerivation, static ? false
, aeson, base, binary, bytestring, data-default-class, directory, hashable
, http-types, mtl, network, scotty, streaming-commons, transformers, unix-time
, unliftio, unordered-containers, warp, hpack, yaml
}:
let config = import ./config.nix {static = static;};
in mkDerivation {
  pname = "metro";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson base binary bytestring data-default-class directory hashable
    http-types mtl network scotty streaming-commons transformers unix-time
    unliftio unordered-containers warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base yaml ];
  testHaskellDepends = [ base binary data-default-class ];
  preConfigure = "hpack";
  homepage = "https://github.com/Lupino/metro#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = config.configureFlags;
}

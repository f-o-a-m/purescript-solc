let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220216/packages.dhall sha256:890466a5e3ed4793ee702d8df8ef85a025fbacbdfeb63c73597aef2795c06845

let foamSpacePackages =
      https://raw.githubusercontent.com/srghma/foam.package-sets/264d157/packages.dhall sha256:070fc3dbb6085fe15b9103cfc8f3194e35448711db7a45f538b3109f03b16139

let overrides = {=}

let additions = {=}

in  upstream // foamSpacePackages // overrides // additions

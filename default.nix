{
  pkgs ? import (import ./pinned.nix) {}
}:

with pkgs;
with builtins;

let
  node_modules = pkgs.callPackage ./node_modules.nix {};
  packageJson = (fromJSON (readFile ./package.json));
  nodejs = pkgs."nodejs-8_x";
  purescript = pkgs.purescript;
in

stdenv.mkDerivation {
  name = packageJson.name;

  buildInputs = [ nodejs purescript psc-package git tesseract ];

  src = builtins.filterSource (path: type:
      type != "unknown" &&
      baseNameOf path != "node_modules" &&
      baseNameOf path != "bower_components" &&
      baseNameOf path != ".git" &&
      baseNameOf path != ".psc-ide-port" &&
      baseNameOf path != ".pulp-cache" &&
      baseNameOf path != ".psc-cache" &&
      baseNameOf path != ".psc-package" &&
      baseNameOf path != "npm.log" &&
      baseNameOf path != "result" &&
      !(pkgs.lib.hasSuffix ".nix" path) &&
      baseNameOf path != "output"
  ) ./.;

  buildPhase = ''
    psc-package install
    psc-package build
    rm -rf src
    ln -s ${node_modules}/node_modules ./
  '';

  installPhase = ''
    mkdir $out
    cp -r . $out/
  '';

  projectDir = toString ./.;

  shellHook = ''
    echo "npm install"
    echo "psc-package build -- --json-errors"
    echo "purs ide server src/**/*.purs .psc-package/psc-0.12.0/**/*.purs"
    export PATH=$projectDir/node_modules/.bin:$PATH
  '';
}

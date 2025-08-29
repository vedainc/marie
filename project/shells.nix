{ nixpkgs, pkgs, ... }:
with pkgs;
rec {
  h = mkShell {
    buildInputs = [
      hello
    ];
  };
  default = h;
}

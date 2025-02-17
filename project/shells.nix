{ nixpkgs, pkgs, ... }:
with pkgs;
rec {
  hello = mkShell {
    buildInputs = [
      hello
    ];
  };
  default = hello;
}

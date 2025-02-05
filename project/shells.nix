{ nixpkgs, pkgs, ... }:
with pkgs; rec {
  lisp = mkShell { buildInputs = [ sbcl ecl ]; };
  default = lisp;
}

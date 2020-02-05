function nixify --description 'Shell function to quickly setup nix + direnv in a new project'
  if test ! -e ./.envrc
    echo "use nix" > .envrc
    direnv allow
  end
  if test ! -e default.nix
    echo >default.nix "\
{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  name = \"env\";
  buildInputs = [
    bashInteractive
  ];
}
"
    $EDITOR default.nix
  end
end

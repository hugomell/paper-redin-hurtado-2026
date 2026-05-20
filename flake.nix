{
  description = "Reprocible shell for bayesian data analysis pipeline";

  inputs = {
    nixpkgs.url ="https://github.com/rstats-on-nix/nixpkgs/archive/2026-04-22.tar.gz";
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";
  in
  {

    devShells."x86_64-linux".default =
      (import ./shell.nix { inherit pkgs; }).shell;
  };
}

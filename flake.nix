{
  description = "Reprocible shell for bayesian data analysis";

  inputs = {
    nixpkgs.url ="https://github.com/rstats-on-nix/nixpkgs/archive/2026-03-09.tar.gz";
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

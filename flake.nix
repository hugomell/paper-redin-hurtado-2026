{
  description = "Reproducible data analysis shell";

  inputs = {
    nixpkgs.url = "https://github.com/rstats-on-nix/nixpkgs/archive/2026-03-02.tar.gz";
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

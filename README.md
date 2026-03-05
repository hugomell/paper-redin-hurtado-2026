
<!--
To preview this README.md file locally:
gh-markdown-preview -p 8000 --markdown-mode README.md
-->

## Reproducible setup

* Open an R shell with the `rix` package using `nix-shell` in a `.rix`
subdirectory at the root of the repository:

```bash
nix-shell -p R rPackages.rix
```

* Get the latest available date for R and Biodonductor releases with
`rix::available_dates()`.

→ `"2026-03-02`


* Execute the cell below to create the `gen-env.R` that will be used to generate
a `default.nix` file for our project using the `rix` package.

```bash
# %% Create gen-env.R

cat << EOF > gen-env.R
library(rix)

rix(
  date = "2026-03-02",
  r_pkgs = c("tidyverse", "bayesplot", "brms", "posterior"),
  py_conf = list(
      py_version = "3.13"
  ),
  git_pkgs = list(
    list(
      package_name = "cmdstanr",
      repo_url = "https://github.com/stan-dev/cmdstanr",
      commit = "da99e2ba954658bdad63bffb738c4444c33a4e0e"
    ),
    list(
      package_name = "httpgd",
      repo_url = "https://github.com/nx10/httpgd",
      commit = "dd6ed3a687a2d7327bb28ca46725a0a203eb2a19"
    ),
    list(
      package_name = "hrbrthemes",
      repo_url = "https://github.com/hrbrmstr/hrbrthemes",
      commit = "d3fd02949fc201c6db616ccaffbb9858aec6fd2b"
    )
  ),
  system_pkgs = "git",
  ide = "radian",
  project_path = ".",
  shell_hook = "
      alias vm='export NVIM_APPNAME='\''nvim-minimal'\''; nvim'
  ",
  overwrite = TRUE
)
EOF
```

* Run the script to get the `default.nix` file:

```bash
# %% Create `default.nix`

Rscript gen-env.R
```

* Use the content in `.rix/default.nix` to create the files `flake.nix` and
`shell.nix` at the root of the repository:

```bash
# %% Create Nix flake files

cat << EOF > flake.nix
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
EOF

cat << 'EOF' > shell.nix
{ pkgs ? import <nixpkgs> {} }:

let
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      bayesplot
      brms
      posterior
      tidyverse;
  };
 
    cmdstanr = (pkgs.rPackages.buildRPackage {
      name = "cmdstanr";
      src = pkgs.fetchgit {
        url = "https://github.com/stan-dev/cmdstanr";
        rev = "da99e2ba954658bdad63bffb738c4444c33a4e0e";
        sha256 = "sha256-wXfOxBexnuL83fvCM+6qv6d7UhTLGq+Xhvja3lRfQpI=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages) 
          checkmate
          data_table
          jsonlite
          posterior
          processx
          R6
          withr
          rlang;
      };
    });

    hrbrthemes = (pkgs.rPackages.buildRPackage {
      name = "hrbrthemes";
      src = pkgs.fetchgit {
        url = "https://github.com/hrbrmstr/hrbrthemes";
        rev = "d3fd02949fc201c6db616ccaffbb9858aec6fd2b";
        sha256 = "sha256-BfclWuD7JsHrscAXO8FmS8239TTYywGcQBp0BnSggYs=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages) 
          ggplot2
          scales
          extrafont
          magrittr
          gdtools;
      };
    });

    httpgd = (pkgs.rPackages.buildRPackage {
      name = "httpgd";
      src = pkgs.fetchgit {
        url = "https://github.com/nx10/httpgd";
        rev = "dd6ed3a687a2d7327bb28ca46725a0a203eb2a19";
        sha256 = "sha256-vs6MTdVJXhTdzPXKqQR+qu1KbhF+vfyzZXIrFsuKMtU=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages) 
          unigd
          cpp11
          AsioHeaders;
      };
    });
   
 
  pyconf = builtins.attrValues {
    inherit (pkgs.python313Packages) 
      pip
      ipykernel;
  };
   
  system_packages = builtins.attrValues {
    inherit (pkgs) 
      git
      glibcLocales
      nix
      python313
      R;
  };
 
  wrapped_pkgs = pkgs.radianWrapper.override {
    packages = [ cmdstanr httpgd hrbrthemes rpkgs ];
  };
 
  shell = pkgs.mkShell {
    LOCALE_ARCHIVE = if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    RETICULATE_PYTHON = "${pkgs.python313}/bin/python";

    buildInputs = [ cmdstanr httpgd hrbrthemes rpkgs pyconf system_packages wrapped_pkgs ];
    shellHook = ''
    
      alias vm='export NVIM_APPNAME='''nvim-minimal'''; nvim'
  
  '';
  }; 
in
  {
    inherit pkgs shell;
  }
EOF
```

<!--
NB: Executing the code cell using slime inserts backslashes that break the
creation of the Nix shell. Instead, use CTRL-x CTRL-e to paste command in Vim
buffer directly from shell.
-->

* Run the Nix development shell with `nix develop` and launch `radian` to
  start an R session.

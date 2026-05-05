{ pkgs ? import <nixpkgs> {} }:

let

 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      bayesplot
      brms
      here
      marginaleffects
      modelsummary
      posterior
      qs2
      quarto
      tarchetypes
      targets
      tidyverse
      usethis
      visNetwork;
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
      cmdstan
      csvkit
      git
      glibcLocales
      nix
      python313
      quarto
      R
      which
      pandoc;
  };
 
  wrapped_pkgs = pkgs.radianWrapper.override {
    packages = [ cmdstanr httpgd hrbrthemes rpkgs  ];
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
    
      export CMDSTAN="${pkgs.cmdstan}/opt/cmdstan"
      # Tell GNU Make NOT to attempt precompiling headers
      export PRECOMPILED_HEADERS="false"

      alias vm='export NVIM_APPNAME='''nvim-minimal'''; nvim'
  
  '';
  }; 
in
  {
    inherit pkgs shell;
  }

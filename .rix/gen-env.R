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

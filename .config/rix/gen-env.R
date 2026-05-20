library(rix)

rix(
  date = "2026-04-22",
  r_pkgs = c(
        "bayesplot",
        "brms",
        "here",
        "marginaleffects",
        "modelsummary",
        "posterior",
        "qs2",
        "quarto",
        "tarchetypes",
        "targets",
        "tidyverse",
        "usethis",
        "visNetwork"
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
  system_pkgs = c("csvkit", "git", "quarto"),
  ide = "radian",
  project_path = ".",
  shell_hook = "
      export CMDSTAN=\"${pkgs.cmdstan}/opt/cmdstan\"
      # Tell GNU Make NOT to attempt precompiling headers
      export PRECOMPILED_HEADERS=\"false\"
  ",
  overwrite = TRUE
)

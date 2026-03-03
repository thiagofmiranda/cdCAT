

usethis::use_vignette("getting-started", title = "Getting Started with cdCAT")

devtools::build_vignettes()

usethis::use_readme_rmd()

devtools::build_readme()

usethis::use_github()
usethis::use_github_action("check-standard")

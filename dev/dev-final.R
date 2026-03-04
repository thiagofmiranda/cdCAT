

usethis::use_vignette("getting-started", title = "Getting Started with cdCAT")
usethis::use_readme_rmd()



usethis::use_github()
usethis::use_github_action("check-standard")

devtools::build_vignettes()
devtools::build_readme()

devtools::document()
devtools::load_all()
devtools::test()
devtools::check()

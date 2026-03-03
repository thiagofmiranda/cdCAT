# Licença MIT
usethis::use_mit_license()

# Configura testthat
usethis::use_testthat()

# Adiciona dependência R6
usethis::use_package("R6")

# Inicializa Git
usethis::use_git()


devtools::check()




# Cria os arquivos R/ do pacote
usethis::use_r("utils")
usethis::use_r("cdcat_items")
usethis::use_r("estimation")
usethis::use_r("criteria")
usethis::use_r("selection")
usethis::use_r("stopping")
usethis::use_r("cdcat_session")


# Cria os arquivos de teste
usethis::use_test("estimation")
usethis::use_test("criteria")
usethis::use_test("selection")
usethis::use_test("cdcat_session")








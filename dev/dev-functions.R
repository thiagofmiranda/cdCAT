devtools::document()   # gera o NAMESPACE com os exports
devtools::load_all()   # recarrega com o NAMESPACE atualizado

usethis::use_build_ignore("dev")

# Teste cdcat_items
Q <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(
  q_matrix = Q,
  model    = "DINA",
  slip     = c(0.1, 0.1, 0.1),
  guess    = c(0.2, 0.2, 0.2)
)
print(items)



# Teste estimation
devtools::load_all()

Q     <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

# Simula respostas: acertou item 1 e 3, errou item 2
est <- estimate_alpha(c(1, 0, 1), items, method = "MAP")

est$alpha_hat        # perfil estimado
est$posterior        # distribuição posterior sobre os 4 perfis



# Teste criteria
devtools::load_all()

# Usando o est criado anteriormente
pm  <- est$prob_matrix
pos <- est$posterior
idx <- est$alpha_hat_index

# Scores dos 3 itens pelo critério PWKL
sapply(1:3, function(j) PWKL_criteria(j, idx, pm, pos))



# Test selection
devtools::load_all()

Q     <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

# Nenhuma resposta ainda — seleciona o primeiro item adaptativo
select_next_item(
  items        = items,
  responses    = c(NA, NA, NA),
  administered = integer(0),
  criterion    = "PWKL"
)

# Após responder item 1 (correto) — qual o próximo?
est <- estimate_alpha(c(1, NA, NA), items, method = "MAP")
select_next_item(
  items        = items,
  responses    = c(1, NA, NA),
  administered = c(1L),
  criterion    = "PWKL",
  est          = est
)




# Test stopping
devtools::load_all()

Q     <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

# Posterior muito concentrada
est_concentrado <- estimate_alpha(c(1, 0, 1), items)

# Anexa skill_patterns ao est para o check_stopping conseguir calcular
sp <- build_skill_patterns(items$n_attrs)
attr(est_concentrado, "skill_patterns") <- sp

check_stopping(est_concentrado, n_administered = 3, threshold = 0.8)



# Test cdcat_session
devtools::load_all()

Q     <- matrix(c(1,0, 0,1, 1,1), nrow = 3, ncol = 2, byrow = TRUE)
items <- cdcat_items(Q, "DINA", slip = c(0.1,0.1,0.1), guess = c(0.2,0.2,0.2))

# Inicia sessão
session <- CdcatSession$new(items, criterion = "PWKL", max_items = 3)

# Loop de aplicação
repeat {
  item <- session$next_item()
  cat("Próximo item:", item, "\n")
  if (item == 0) break

  # Simula resposta (1 = correto)
  session$update(item, 1)
}

# Resultado final
session$result()



# Test tests
devtools::test()


## complete check
devtools::check()


# Documenta tudo uma última vez
devtools::document()

# Commit inicial
usethis::use_git()







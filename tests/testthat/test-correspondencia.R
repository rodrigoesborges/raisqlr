test_that("equivalentes preferenciais dos setores usados pelo painel PNDR", {
  expect_identical(cnae_equivalentes(72, "2.0", "1.0"), 73L)      # P&D
  expect_identical(cnae_equivalentes(84, "2.0", "95"), 75L)       # adm publica
  expect_identical(cnae_equivalentes(38, "2.0", "1.0"), c(37L, 45L, 90L)) # residuos
  expect_identical(cnae_equivalentes(c(1, 2, 3), "2.0", "1.0"), c(1L, 2L, 5L)) # agro
  expect_identical(cnae_equivalentes(5:9, "2.0", "1.0"), c(10L, 11L, 13L, 14L)) # mineracao
})

test_that("sentido reverso 1.0 -> 2.0", {
  expect_identical(cnae_equivalentes(75, "1.0", "2.0"), 84L)
  expect_identical(cnae_equivalentes(73, "95", "2.0"), 72L)
})

test_that("nivel grupo agrega classes", {
  # biotec/saude (2.0: 211,212,266,325) -> quimica + instrumentos no 1.0
  expect_identical(
    cnae_equivalentes(c(211, 212, 266, 325), "2.0", "1.0", "grupo"),
    c(233L, 245L, 331L))
})

test_that("preferencial = FALSE inclui conteudo parcial", {
  expect_true(length(cnae_equivalentes(38, "2.0", "1.0", preferencial = FALSE)) > 2)
})

test_that("erros esperados", {
  expect_error(cnae_equivalentes(72, "2.0", "2.0"), "diferentes")
  expect_error(cnae_equivalentes(72, "2.3", "1.0"), "versao")
})

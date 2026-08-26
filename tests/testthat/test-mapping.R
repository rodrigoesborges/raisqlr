test_that("map_headers converte o dialeto novo para o contrato legado", {
  raw2025 <- c(
    "Bairros SP - C\u00f3digo", "Bairros Fortaleza - C\u00f3digo",
    "CBO 2002 Ocupa\u00e7\u00e3o - C\u00f3digo", "Ind V\u00ednculo Ativo 31/12 - C\u00f3digo",
    "Munic\u00edpio - C\u00f3digo", "Munic\u00edpio Trab - C\u00f3digo",
    "Sexo - C\u00f3digo", "Vl Rem Dezembro Nom", "Vl Rem M\u00e9dia (SM)",
    "Faixa Rem M\u00e9dia (SM) - C\u00f3digo", "Faixa Rem Dez (SM) - C\u00f3digo",
    "Ind Estabelecimento Participante SIMPLES - C\u00f3digo",
    "Ind Trab Intermitente - C\u00f3digo", "Tipo Estabelecimento - Nome",
    "Qtd Hora Contr", "Tempo Emprego")
  got <- raisqlr:::map_headers(raw2025)
  expect_identical(
    got,
    c("bairros_sp", "bairros_fortaleza", "cbo_ocupacao_2002", "vinculo_ativo_31_12",
      "municipio", "mun_trab", "sexo_trabalhador", "vl_remun_dezembro_nom",
      "vl_remun_media__sm_", "faixa_remun_media__sm_", "faixa_remun_dezem__sm_",
      "ind_simples", "ind_trab_intermitente", "tipo_estab_1",
      "qtd_hora_contr", "tempo_emprego"))
})

test_that("map_headers e estavel no dialeto legado", {
  raw <- c("Municipio", "Vinculo Ativo 31/12", "CBO Ocupacao 2002", "Qtd Hora Contr")
  expect_identical(
    raisqlr:::map_headers(raw),
    c("municipio", "vinculo_ativo_31_12", "cbo_ocupacao_2002", "qtd_hora_contr"))
})

test_that("guess_sep detecta virgula vs ponto-e-virgula", {
  expect_identical(raisqlr:::guess_sep('"A, B",10,"x"'), ",")
  expect_identical(raisqlr:::guess_sep("a;b;c"), ";")
})

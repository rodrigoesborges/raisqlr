test_that("rais_catalog cataloga diretorio local com estrutura de anos", {
  d <- file.path(tempdir(), "raisqlr_fix")
  dir.create(file.path(d, "2025"), recursive = TRUE, showWarnings = FALSE)
  file.copy(fixture, file.path(d, "2025", "RAIS_VINC_PUB_NI.7z"))
  cat <- rais_catalog(dir = d)
  expect_equal(nrow(cat), 1)
  expect_equal(cat$subtype, "vinculo")
  expect_equal(cat$year, 2025)
  expect_equal(cat$situacao, "definitiva")  # pasta sem sufixo: ver doc da funcao
  expect_equal(cat$db_tablename, "rais_vinculo_2025")
})

test_that("rais_build converte a fixture NI para o dialeto contratado", {
  d <- file.path(tempdir(), "raisqlr_build")
  cat <- data.frame(
    full_path = fixture, file = basename(fixture), folder = "2025",
    subtype = "vinculo", year = 2025, situacao = "definitiva",
    db_tablename = "rais_vinculo_2025", size_bytes = file.size(fixture),
    stringsAsFactors = FALSE)
  cache <- rais_build(cat, dir = d, tmpdir = d)
  expect_true(file.exists(cache$output_filename))
  expect_gt(cache$nlines, 0)
  hdr <- readLines(gzfile(cache$output_filename), n = 1)
  cols <- strsplit(hdr, ";", fixed = TRUE)[[1]]
  expect_true(all(c("municipio", "vinculo_ativo_31_12", "cbo_ocupacao_2002",
                    "vl_remun_dezembro_nom", "ind_trab_intermitente") %in% cols))
  expect_false(any(grepl("codigo", cols)))
  # valores numericos com espaco/esvaziados viram NA ou numero, nao texto
  x <- raisqlr:::fread_safe(cache$output_filename, sep = ";", dec = ".", nrows = 100)
  expect_true(is.numeric(x$cbo_ocupacao_2002) || is.integer(x$cbo_ocupacao_2002))
})

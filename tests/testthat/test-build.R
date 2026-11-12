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

test_that("rais_build esvazia placeholders '0*-1' e mantem coluna toda-NA numerica (IGNORADO 1994-97)", {
  d <- file.path(tempdir(), "raisqlr_build_ph")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  ign <- make_legacy_vinc_7z(file.path(d, "IGNORADO1994.7z"), "IGNORADO1994.TXT",
                             c("0000-1", "0000-1"))
  misto <- make_legacy_vinc_7z(file.path(d, "RJSUL1994.7z"), "RJSUL1994.TXT",
                               c("0000-1", "110020"))
  cat2 <- data.frame(
    full_path = c(ign, misto), file = basename(c(ign, misto)),
    folder = "1994", subtype = "vinculo", year = 1994, situacao = "definitiva",
    db_tablename = "rais_vinculo_1994",
    size_bytes = file.size(c(ign, misto)), stringsAsFactors = FALSE)
  cache <- rais_build(cat2, dir = d, tmpdir = d)
  expect_true(all(file.exists(cache$output_filename)))

  # arquivo misto: placeholder some e o codigo real fica, coluna numerica
  x <- raisqlr:::fread_safe(cache$output_filename[2], sep = ";", dec = ".", nrows = 10)
  expect_true(is.numeric(x$municipio))
  expect_equal(sum(is.na(x$municipio)), 1)
  expect_equal(x$municipio[!is.na(x$municipio)], 110020)

  # arquivo so-placeholder: nada de texto '0000-1' no gz, coluna toda-NA nao-e-char
  y <- raisqlr:::fread_safe(cache$output_filename[1], sep = ";", dec = ".", nrows = 10)
  expect_false(is.character(y$municipio))
  expect_true(all(is.na(y$municipio)))
  linhas <- readLines(gzfile(cache$output_filename[1]), n = 4)
  expect_false(any(grepl("0000-1", linhas, fixed = TRUE)))
})

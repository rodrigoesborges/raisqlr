test_that("rais_pg_load + narrow + indexes ciclo completo (requer pwdrais + banco local mte_rais)", {
  skip_if(!has_db, "sem credenciais (pwdrais)")
  skip_if(Sys.getenv("RAISQLR_TEST_DB", "1") == "0", "testes de banco desativados")

  d <- file.path(tempdir(), "raisqlr_pg")
  cat <- data.frame(
    full_path = fixture, file = basename(fixture), folder = "2025",
    subtype = "vinculo", year = 2025, situacao = "definitiva",
    db_tablename = "rais_vinculo_2098", size_bytes = file.size(fixture),
    stringsAsFactors = FALSE)
  cache <- rais_build(cat, dir = d, tmpdir = d)
  cache$db_tablename <- "rais_vinculo_2098"
  on.exit(try({
    db <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mte_rais", host = "localhost",
                         user = "mte_rais", password = Sys.getenv("pwdrais"))
    DBI::dbExecute(db, "DROP TABLE IF EXISTS rais_vinculo_2098")
    DBI::dbDisconnect(db)
  }, silent = TRUE), add = TRUE)

  rais_pg_load(cache)
  db <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mte_rais", host = "localhost",
                       user = "mte_rais", password = Sys.getenv("pwdrais"))
  n <- DBI::dbGetQuery(db, "SELECT count(*) n FROM rais_vinculo_2098")$n
  expect_gte(n, 1)
  DBI::dbDisconnect(db)

  rais_narrow_types("rais_vinculo_2098")
  db <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mte_rais", host = "localhost",
                       user = "mte_rais", password = Sys.getenv("pwdrais"))
  ty <- DBI::dbGetQuery(db, "SELECT data_type FROM information_schema.columns
                            WHERE table_name='rais_vinculo_2098' AND column_name='sexo_trabalhador'")
  expect_identical(ty$data_type, "smallint")
  DBI::dbExecute(db, "DROP TABLE rais_vinculo_2098")
  DBI::dbDisconnect(db)
})

test_that("rais_pg_load tipa coluna toda-NA como double precision (IGNORADO 1994-97)", {
  skip_if(!has_db, "sem credenciais (pwdrais)")
  skip_if(Sys.getenv("RAISQLR_TEST_DB", "1") == "0", "testes de banco desativados")

  d <- file.path(tempdir(), "raisqlr_pg_ph")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  ign <- make_legacy_vinc_7z(file.path(d, "IGNORADO1994.7z"), "IGNORADO1994.TXT",
                             c("0000-1", "0000-1"))
  misto <- make_legacy_vinc_7z(file.path(d, "RJSUL1994.7z"), "RJSUL1994.TXT",
                               c("0000-1", "110020"))
  cat2 <- data.frame(
    full_path = c(ign, misto), file = basename(c(ign, misto)),
    folder = "1994", subtype = "vinculo", year = 1994, situacao = "definitiva",
    db_tablename = "rais_vinculo_2094",
    size_bytes = file.size(c(ign, misto)), stringsAsFactors = FALSE)
  cache <- rais_build(cat2, dir = d, tmpdir = d)
  cache$db_tablename <- "rais_vinculo_2094"
  on.exit(try({
    db <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mte_rais", host = "localhost",
                         user = "mte_rais", password = Sys.getenv("pwdrais"))
    DBI::dbExecute(db, "DROP TABLE IF EXISTS rais_vinculo_2094")
    DBI::dbDisconnect(db)
  }, silent = TRUE), add = TRUE)

  rais_pg_load(cache)
  db <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mte_rais", host = "localhost",
                       user = "mte_rais", password = Sys.getenv("pwdrais"))
  ty <- DBI::dbGetQuery(db, "SELECT data_type FROM information_schema.columns
                            WHERE table_name='rais_vinculo_2094' AND column_name='municipio'")
  expect_identical(ty$data_type, "double precision")
  v <- DBI::dbGetQuery(db, "SELECT municipio FROM rais_vinculo_2094")
  expect_equal(sum(is.na(v$municipio)), 3)
  expect_equal(v$municipio[!is.na(v$municipio)], 110020)
  DBI::dbExecute(db, "DROP TABLE rais_vinculo_2094")
  DBI::dbDisconnect(db)
})

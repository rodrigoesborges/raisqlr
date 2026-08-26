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

#' Cria os indices de consulta das tabelas RAIS
#'
#' Replica o conjunto historico de indices do banco `mte_rais` (criados para
#' as queries de indicadores do AEDi/PNDR): nos vinculos,
#' `(vinculo_ativo_31_12, municipio)` e compostos com `cnae_2_0_classe`,
#' `escolaridade_apos_2005`, `trunc(cbo_ocupacao_2002/1000)` e
#' `trunc(cbo_ocupacao_2002)`; nos estabelecimentos, `(municipio)` e
#' `(municipio, cnae_2_0_classe)`.
#'
#' @param years anos; default: todos os `rais_vinculo_<ano>` existentes.
#' @param concurrently usar CREATE INDEX CONCURRENTLY (default FALSE; exige
#'   fora de transacao e so faz sentido com consumidores ativos).
#' @param dbname,host,user,password,port,db conexao (mesma convencao de
#'   [rais_pg_load()]).
#' @export
rais_create_indexes <- function(years = NULL, concurrently = FALSE,
                                dbname = Sys.getenv("dbrais", "mte_rais"),
                                host = Sys.getenv("hostraispsql", "localhost"),
                                user = Sys.getenv("mte_rais", "mte_rais"),
                                password = Sys.getenv("pwdrais"),
                                port = 5432, db = NULL) {
  if (is.null(db)) {
    if (!nzchar(password)) stop("senha ausente: informe `password` ou defina pwdrais")
    db <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host,
                         user = user, password = password, port = port)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
  }
  if (is.null(years)) {
    tabs <- DBI::dbGetQuery(db, "SELECT table_name FROM information_schema.tables
                                WHERE table_schema='public' AND table_name ~ '^rais_'")$table_name
    years <- unique(as.numeric(gsub("\\D", "", tabs)))
  }
  ci <- if (concurrently) "CREATE INDEX CONCURRENTLY IF NOT EXISTS" else "CREATE INDEX IF NOT EXISTS"
  for (y in years) {
    q <- c(
      sprintf("%s idx_rais_vinculo_%d_municipio ON rais_vinculo_%d (vinculo_ativo_31_12, municipio)", ci, y, y),
      sprintf("%s idx_rais_vinculo_%d_municipio_cnae ON rais_vinculo_%d (vinculo_ativo_31_12, municipio, cnae_2_0_classe)", ci, y, y),
      sprintf("%s idx_rais_vinculo_%d_municipio_escolaridade ON rais_vinculo_%d (vinculo_ativo_31_12, municipio, escolaridade_apos_2005)", ci, y, y),
      sprintf("%s idx_rais_vinculo_%d_municipio_cbo ON rais_vinculo_%d (vinculo_ativo_31_12, municipio, trunc(cbo_ocupacao_2002/1000))", ci, y, y),
      sprintf("%s idx_rais_vinculo_%d_municipio_cbog ON rais_vinculo_%d (vinculo_ativo_31_12, municipio, trunc(cbo_ocupacao_2002))", ci, y, y),
      sprintf("%s idx_rais_estabelecimento_%d_municipio ON rais_estabelecimento_%d (municipio)", ci, y, y),
      sprintf("%s idx_rais_estabelecimento_%d_municipio_cnae ON rais_estabelecimento_%d (municipio, cnae_2_0_classe)", ci, y, y))
    for (stmt in q) DBI::dbExecute(db, stmt)
    DBI::dbExecute(db, sprintf("ANALYZE rais_vinculo_%d", y))
    DBI::dbExecute(db, sprintf("ANALYZE rais_estabelecimento_%d", y))
  }
  invisible(years)
}

#' Reduz tipos das colunas categoricas das tabelas RAIS
#'
#' Colunas `double precision`/`integer` que nao se enquadram na lista de
#' excecoes (`vl_rem*`, `tempo_emprego`, `qtd_hora_contr`) sao reescritas
#' como `smallint` (<= 32767 em valor absoluto) ou `integer`, apos uma unica
#' varredura de magnitude por tabela; a coluna `row.names` (artefato R do
#' carregamento legado) e removida. Nomes de colunas nunca mudam (contrato
#' AEDi). No banco 620 GB -> 325 GB (v0 -> v0.1, ago/2026).
#'
#' @param tables vetores de nomes de tabela; default: todas as `rais_*`.
#' @inheritParams rais_create_indexes
#' @export
rais_narrow_types <- function(tables = NULL,
                              dbname = Sys.getenv("dbrais", "mte_rais"),
                              host = Sys.getenv("hostraispsql", "localhost"),
                              user = Sys.getenv("mte_rais", "mte_rais"),
                              password = Sys.getenv("pwdrais"),
                              port = 5432, db = NULL) {
  if (is.null(db)) {
    if (!nzchar(password)) stop("senha ausente: informe `password` ou defina pwdrais")
    db <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host,
                         user = user, password = password, port = port)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
  }
  if (is.null(tables)) {
    tables <- DBI::dbGetQuery(db, "SELECT table_name FROM information_schema.tables
                                  WHERE table_schema='public' AND table_name ~ '^rais_'")$table_name
  }
  for (t in tables) {
    cols <- DBI::dbGetQuery(db, sprintf(
      "SELECT column_name FROM information_schema.columns
       WHERE table_name='%s' AND data_type IN ('double precision','integer')
         AND column_name !~ '%s' ORDER BY ordinal_position", t, KEEP_DOUBLE_RX))$column_name
    DBI::dbExecute(db, sprintf("ALTER TABLE %s DROP COLUMN IF EXISTS \"row.names\"", t))
    if (!length(cols)) next
    mag <- DBI::dbGetQuery(db, sprintf(
      "SELECT %s FROM %s",
      paste(sprintf("greatest(abs(min(%s)),abs(max(%s))) AS m_%s", cols, cols, cols), collapse = ","),
      t))
    alters <- character(0)
    for (cc in cols) {
      m <- mag[[paste0("m_", cc)]]
      ty <- if (is.na(m) || abs(m) <= 32767) "smallint" else if (abs(m) <= 2147483647) "integer" else NA
      if (!is.na(ty))
        alters <- c(alters, sprintf("ALTER COLUMN %s TYPE %s USING %s::%s", cc, ty, cc, ty))
    }
    if (length(alters)) {
      message(t, ": estreitando ", length(alters), " colunas")
      DBI::dbExecute(db, sprintf("ALTER TABLE %s %s", t, paste(alters, collapse = ", ")))
    }
    DBI::dbExecute(db, sprintf("ANALYZE %s", t))
  }
  invisible(tables)
}

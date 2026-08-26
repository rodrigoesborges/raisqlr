#' Carrega o cache da RAIS em PostgreSQL
#'
#' Cria (ou substitui) as tabelas `rais_vinculo_<ano>`/`rais_estabelecimento_<ano>`
#' no PostgreSQL e as popula por blocos via COPY. A estrutura da tabela e
#' harmonizada entre os arquivos do mesmo ano (uniao das colunas; coluna e
#' texto se algum arquivo a tipa como texto, numerica caso contrario).
#' Cada tabela e escrita numa unica transacao (ROLLBACK automatico em erro).
#'
#' @details Contrato congelado (consumido por `AEDi/R/upload_raispsql.R`):
#' tabelas `rais_vinculo_YYYY` e `rais_estabelecimento_YYYY` no schema
#' `public`, nomes de colunas legados (veja [rais_build()]). Use
#' [rais_narrow_types()] apos a carga para reduzir codigos categoricos de
#' `double precision`/`integer` para `smallint`/`integer`.
#'
#' Credenciais: por argumento ou, em ordem de precedencia, pelas variaveis de
#' ambiente compartilhadas com o AEDi: `dbrais` (banco, default `"mte_rais"`),
#' `mte_rais` (usuario, default `"mte_rais"`), `pwdrais` (senha),
#' `hostraispsql` (host, default `"localhost"`).
#'
#' @param catalog catalogo com `output_filename` (saida de [rais_build()]).
#' @param replace logico (default TRUE): faz DROP + CREATE da tabela; com
#'   FALSE exige tabela vazia/ausente e apenas acrescenta (append).
#' @param dbname,host,user,password,port conexao (ver Details).
#' @param chunk.size linhas por bloco (default 1e6).
#' @param db conexao DBI alternativa ja aberta (tem precedencia sobre os
#'   argumentos de conexao).
#'
#' @return catalogo de entrada (invisivel).
#' @export
rais_pg_load <- function(catalog, replace = TRUE,
                         dbname = Sys.getenv("dbrais", "mte_rais"),
                         host = Sys.getenv("hostraispsql", "localhost"),
                         user = Sys.getenv("mte_rais", "mte_rais"),
                         password = Sys.getenv("pwdrais"),
                         port = 5432, chunk.size = as.integer(1e6),
                         db = NULL) {
  stopifnot(is.data.frame(catalog), "output_filename" %in% names(catalog))
  if (is.null(db)) {
    if (!nzchar(password)) stop("senha ausente: informe `password` ou defina pwdrais")
    db <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host,
                         user = user, password = password, port = port)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
  }

  for (tbl in split(catalog, catalog$db_tablename)) {
    tname <- tbl$db_tablename[1]
    message("carregando ", tname, " (", nrow(tbl), " arquivo(s))")

    # harmonized structure across the files of this table. Numeric columns
    # are ALWAYS double precision at load time (a 1-row sample cannot
    # reliably distinguish integer/double - COPY aborts on "1.0000000" into
    # integer); rais_narrow_types() narrows afterwards with a full scan.
    structs <- lapply(tbl$output_filename, function(f) {
      x <- fread_safe(f, sep = ";", dec = ".", nrows = 1)
      data.frame(column = names(x), class = vapply(x, function(z) class(z)[1], ""))
    })
    all_cols <- unique(do.call(rbind, structs))
    fmt <- tapply(all_cols$class, all_cols$column, function(k)
      if (any(k %in% c("character", "Date", "logical"))) "text" else "double precision")
    cols <- structs[[1]]$column  # preserves first file's order
    cols <- cols[cols %in% names(fmt)]
    extra <- setdiff(names(fmt), cols)
    if (length(extra)) cols <- c(cols, extra)

    DBI::dbBegin(db)
    tryCatch({
      if (replace)
        DBI::dbExecute(db, paste0("DROP TABLE IF EXISTS ", DBI::dbQuoteIdentifier(db, tname)))
      ddl <- paste0("CREATE TABLE ", DBI::dbQuoteIdentifier(db, tname), " (\n  ",
        paste(DBI::dbQuoteIdentifier(db, cols), fmt[cols], collapse = ",\n  "), ")")
      DBI::dbExecute(db, ddl)

      for (fi in seq_len(nrow(tbl))) {
        f <- tbl$output_filename[fi]
        nlines <- nrow(fread_safe(f, sep = ";", dec = ".", header = TRUE,
                                 select = 1L, showProgress = FALSE))
        these_cols <- strsplit(read_head(f), ";", fixed = TRUE)[[1]]

        for (start in seq(1, max(nlines, 1), by = chunk.size)) {
          x <- fread_safe(f, sep = ";", dec = ".", header = FALSE,
                          encoding = "Latin-1", skip = start,
                          nrows = chunk.size, showProgress = FALSE)
          data.table::setnames(x, these_cols)
          missing <- setdiff(cols, names(x))
          if (length(missing)) x[, (missing) := NA]
          for (cc in names(fmt)[fmt == "double precision"]) {
            v <- x[[cc]]
            if (!is.null(v) && is.character(v))
              data.table::set(x, j = cc, value = suppressWarnings(as.numeric(trimws(v))))
          }
          data.table::setcolorder(x, cols)
          DBI::dbAppendTable(db, DBI::dbQuoteIdentifier(db, tname), as.data.frame(x))
          rm(x); gc()
        }
      }
      DBI::dbCommit(db)
    }, error = function(e) {
      try(DBI::dbRollback(db), silent = TRUE)
      stop("falha ao carregar ", tname, ": ", conditionMessage(e))
    })
  }
  invisible(catalog)
}

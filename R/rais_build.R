#' Converte os microdados da RAIS para o dialeto contratado
#'
#' Extrai os `.7z`, detecta o dialeto (legado: `sep=";"`, `dec=","`,
#' ISO-8859-1; novo, desde a RAIS 2024 definitiva: `sep=","`, `dec="."`,
#' cabecalhos `"X - Codigo"`, arquivos internos `*.COMT`), normaliza os nomes
#' de colunas para o contrato congelado consumido pelo AEDi
#' (`rais_vinculo_YYYY`/`rais_estabelecimento_YYYY`), aplica `-1 -> NA` e a
#' correcao historica de CBO (`'0000-1' -> '00001'`, RAIS 2020), e grava o
#' cache em CSV gzip `sep=";"`/`dec="."` pronto para [rais_pg_load()].
#'
#' @param catalog catalogo com coluna `local_file` (saida de [rais_download()];
#'   sem ela, `full_path` precisa apontar para arquivo local).
#' @param dir diretorio do cache de saida (default `"rais_cache"`).
#' @param chunk.size linhas por bloco (default 1e6).
#' @param tmpdir diretorio para temporarios (default `tempdir()`).
#'
#' @return catalogo acrescido de `output_filename` (o `.gz` produzido) e
#'   `nlines`.
#' @export
rais_build <- function(catalog, dir = "rais_cache", chunk.size = as.integer(1e6),
                       tmpdir = tempdir()) {
  stopifnot(is.data.frame(catalog))
  src <- if ("local_file" %in% names(catalog)) catalog$local_file else catalog$full_path
  catalog$output_filename <- file.path(dir, paste0(catalog$year, "/",
    sub("\\.(7z|zip)$", "", catalog$file, ignore.case = TRUE), ".gz"))
  catalog$nlines <- NA_integer_

  for (i in seq_len(nrow(catalog))) {
    if (file.exists(catalog$output_filename[i])) next
    dir.create(dirname(catalog$output_filename[i]), recursive = TRUE, showWarnings = FALSE)
    td <- file.path(tmpdir, paste0("raisqlr_unzip_", i))
    unlink(td, recursive = TRUE); dir.create(td, recursive = TRUE, showWarnings = FALSE)

    archive::archive_extract(normalizePath(src[i]), dir = td)
    f <- list.files(td, full.names = TRUE)
    f <- f[file.info(f)$size > 0][1]

    # detect dialect from the header line
    hdr_con <- file(f, encoding = "LATIN1")
    hdr_line <- readLines(hdr_con, n = 1L); close(hdr_con)
    sep <- guess_sep(hdr_line)

    new_names <- map_headers(
      strsplit(hdr_line, sep, fixed = TRUE)[[1]] |> (\(x) gsub('^"|"$', "", x))())
    new_names <- make.unique(new_names, sep = "_")
    if (anyDuplicated(new_names)) stop("colunas duplicadas apos mapeamento: ", basename(src[i]))

    nlines <- nrow(data.table::fread(f, sep = sep, header = TRUE, encoding = "Latin-1",
                                     select = 1L, showProgress = FALSE, fill = TRUE))
    nchunks <- ceiling(nlines / chunk.size)
    tf2 <- tempfile(tmpdir = tmpdir)
    start_n <- 1
    for (ch in seq_len(nchunks)) {
      x <- data.table::fread(f, sep = sep, dec = if (sep == ",") "." else ",",
                             header = FALSE, encoding = "Latin-1",
                             skip = start_n, nrows = chunk.size, showProgress = FALSE,
                             fill = TRUE)
      data.table::setnames(x, new_names)
      for (j in names(x)) {
        nas <- which(x[[j]] == -1)
        if (length(nas)) data.table::set(x, i = nas, j = j, value = NA)
        if (is.character(x[[j]]) && j != "tipo_estab_1") {
          if (j == "cbo_ocupacao_2002")
            data.table::set(x, j = j, value = gsub("0000-1", "00001", x[[j]]))
          v <- suppressWarnings(as.numeric(trimws(x[[j]])))
          if (any(!is.na(v))) data.table::set(x, j = j, value = v)
        }
      }
      data.table::fwrite(x, tf2, append = TRUE, sep = ";", dec = ".",
                         showProgress = FALSE, compress = "gzip")
      rm(x); gc()
      start_n <- start_n + chunk.size
    }
    if (!suppressWarnings(file.rename(tf2, catalog$output_filename[i]))) {
      if (!file.copy(tf2, catalog$output_filename[i], overwrite = TRUE))
        stop("falha ao gravar ", catalog$output_filename[i])
      unlink(tf2)
    }
    unlink(td, recursive = TRUE)
    catalog$nlines[i] <- nlines
    message(basename(src[i]), " -> ", catalog$output_filename[i], " (", nlines, " linhas)")
  }
  catalog
}

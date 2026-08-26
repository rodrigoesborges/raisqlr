#' Layouts (dicionarios de variaveis) da RAIS
#'
#' Baixa do FTP oficial (`Layouts/`) e le os XLS de layout de estabelecimentos
#' e vinculos, com as colunas padronizadas `Nome` e `Descricao da Variavel`
#' usadas pelo modulo `upload_raispsql` do AEDi (via `lowdash()` local).
#'
#' @param subtype `"vinculo"` (default) ou `"estabelecimento"`.
#' @param dir diretorio de cache (default `"rais_cache/layouts"`).
#' @param root_url raiz do FTP.
#'
#' @return `data.frame` com `Nome`, `Descricao da Variavel` (e demais colunas
#'   do XLS original).
#' @export
rais_layout <- function(subtype = c("vinculo", "estabelecimento"),
                        dir = "rais_cache/layouts",
                        root_url = "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS") {
  subtype <- match.arg(subtype)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  entries <- ftp_list_dir(file.path(root_url, "Layouts"))
  xl <- grep("xls", entries$name, ignore.case = TRUE, value = TRUE)
  pick <- if (subtype == "vinculo") {
    v <- xl[grepl("vinc", xl, ignore.case = TRUE)]
    if (length(v)) v else xl[grepl("v[0-9]", tolower(xl))]
  } else {
    v <- xl[grepl("estab", xl, ignore.case = TRUE)]
    if (length(v)) v else setdiff(xl, xl[grepl("vinc", xl, ignore.case = TRUE)])
  }
  if (!length(pick)) stop("layout XLS nao localizado no FTP para ", subtype)
  pick <- pick[order(entries$size[match(pick, entries$name)])]
  pick <- pick[length(pick)]

  dest <- file.path(dir, pick)
  if (!file.exists(dest)) curl::curl_download(file.path(root_url, "Layouts", pick), dest, mode = "wb")

  df <- readxl::read_excel(dest, skip = 1)
  df <- as.data.frame(df)
  names(df) <- iconv(tolower(gsub("[ ()-]+", "_", names(df))), to = "ASCII//TRANSLIT")
  df <- df[nzchar(df$nome %||% ""), , drop = FALSE]
  df
}

`%||%` <- function(a, b) if (is.null(a)) b else a

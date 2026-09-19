#' Cataloga os microdados da RAIS disponiveis
#'
#' Lista os arquivos de microdados (vinculos e estabelecimentos) diretamente
#' do FTP oficial do MTE, ou de um diretorio local com os arquivos `.7z`
#' baixados. O catalogo alimenta [rais_download()], [rais_build()] e
#' [rais_pg_load()].
#'
#' @param years vetor numerico de anos a reter (default: todos).
#' @param situacao `"definitiva"`, `"parcial"` ou `"qualquer"` (default).
#'   A situacao e derivada do nome da pasta do FTP: pastas `"<ano> Parcial"`
#'   sao preliminares. ATENCAO: desde a RAIS 2025 o MTE publica o release
#'   preliminar direto na pasta `"<ano>"` (sem sufixo); nesses casos a
#'   deteccao por nome nao e possivel - verifique o Comunicado na pasta.
#' @param root_url raiz do FTP (default oficial).
#' @param dir alternativamente, diretorio local contendo `<ano>/*.7z`
#'   (estrutura espelhando o FTP) ou os `.7z` soltos; quando informado,
#'   o FTP nao e consultado.
#' @param recursive logico; varre subpastas de `dir` recursivamente.
#'
#' @return `data.frame` com colunas `full_path`, `file`, `subtype`
#'   (`"vinculo"`/`"estabelecimento"`), `year` (numerico), `situacao`,
#'   `size_bytes` (NA quando local), `folder` e `db_tablename`
#'   (`rais_<subtype>_<year>`).
#' @export
#'
#' @examples
#' \dontrun{
#' cat <- rais_catalog(years = 2024:2025)
#' rais_download(cat, dir = "rais_data")
#' cache <- rais_build(cat, dir = "rais_cache")
#' rais_pg_load(cache)
#' }
rais_catalog <- function(years = NULL,
                         situacao = c("qualquer", "definitiva", "parcial"),
                         root_url = "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS",
                         dir = NULL,
                         recursive = TRUE) {
  situacao <- match.arg(situacao)

  if (is.null(dir)) {
    entries <- ftp_list_dir(root_url)
    folders <- entries$name[entries$isdir]
    year_folders <- folders[grepl("^[0-9]{4}( Parcial)?$", folders)]
    if (!length(year_folders)) stop("nenhuma pasta anual encontrada em ", root_url)
    out <- do.call(rbind, lapply(year_folders, function(f) {
      sub <- ftp_list_dir(file.path(root_url, f))
      sub <- sub[!sub$isdir, ]
      if (!nrow(sub)) return(NULL)
      data.frame(
        full_path = file.path(root_url, f, sub$name),
        file      = sub$name,
        size_bytes = sub$size,
        folder    = f,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    allf <- list.files(dir, pattern = "\\.(7z|zip)$", full.names = TRUE,
                       recursive = recursive, ignore.case = TRUE)
    if (!length(allf)) stop("nenhum .7z/.zip sob ", dir)
    out <- data.frame(
      full_path = allf,
      file      = basename(allf),
      size_bytes = file.size(allf),
      folder    = basename(dirname(allf)),
      stringsAsFactors = FALSE
    )
  }

  out <- out[grepl("\\.(7z|zip)$", out$file, ignore.case = TRUE), , drop = FALSE]
  out <- out[!grepl("cumulado", out$file, ignore.case = TRUE), , drop = FALSE]

  out$subtype <- ifelse(
    grepl("estb|estabelecimento|estab", out$full_path, ignore.case = TRUE),
    "estabelecimento", "vinculo")

  yr <- gsub("\\D+", "", out$folder)
  bad <- nchar(yr) > 4
  if (any(bad)) yr[bad] <- substr(yr[bad], 1, 4)
  out$year <- suppressWarnings(as.numeric(yr))
  out <- out[!is.na(out$year), , drop = FALSE]

  out$situacao <- ifelse(grepl("parcial", out$folder, ignore.case = TRUE),
                         "parcial", "definitiva")

  if (!is.null(years)) out <- out[out$year %in% years, , drop = FALSE]
  if (situacao != "qualquer") out <- out[out$situacao == situacao, , drop = FALSE]
  if (!nrow(out)) stop("catalogo vazio apos filtros (years/situacao)")

  out$db_tablename <- paste0("rais_", out$subtype, "_", out$year)
  rownames(out) <- NULL
  out[, c("full_path", "file", "folder", "subtype", "year", "situacao",
          "db_tablename", "size_bytes")]
}

# minimal FTP directory listing over the curl package (Windows-style lines:
# "MM-DD-YY  HH:MM<AM/PM>  <DIR>|<size>  name with spaces")
ftp_list_dir <- function(url) {
  con <- curl::curl(paste0(URLencode(sub("/$", "", url)), "/"), "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  bad <- is.na(iconv(lines, "UTF-8", "UTF-8"))
  if (any(bad)) lines[bad] <- iconv(lines[bad], "latin1", "UTF-8")
  m <- regmatches(lines, regexec("^\\S+\\s+\\S+\\s+(<DIR>|[0-9]+)\\s+(.*)$", lines))
  ok <- lengths(m) == 3
  if (!any(ok)) stop("listagem FTP vazia ou em formato nao reconhecido: ", url)
  size_token <- vapply(m[ok], `[[`, character(1), 2)
  data.frame(
    name  = vapply(m[ok], `[[`, character(1), 3),
    size  = suppressWarnings(as.numeric(size_token)),
    isdir = size_token == "<DIR>",
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# internal helpers shared by build/convert -----------------------------------

remove_special_character <- function(x) {
  stripped_x <- iconv(x, to = "ascii//translit", sub = "_")
  stripped_x <- gsub("('|~|\\^)", "", stripped_x, ignore.case = TRUE)
  stripped_x
}

# normalized-new-dialect header -> legacy contracted column name
new2legacy <- c(
  "ind_vinculo_ativo_31_12"          = "vinculo_ativo_31_12",
  "faixa_rem_media__sm_"             = "faixa_remun_media__sm_",
  "faixa_rem_dez__sm_"               = "faixa_remun_dezem__sm_",
  "vl_rem_dezembro__sm_"             = "vl_remun_dezembro__sm_",
  "vl_rem_media__sm_"                = "vl_remun_media__sm_",
  "vl_rem_dezembro_nom"              = "vl_remun_dezembro_nom",
  "vl_rem_media_nom"                 = "vl_remun_media_nom",
  "sexo"                             = "sexo_trabalhador",
  "regiao_adm_df"                    = "regioes_adm_df",
  "municipio_trab"                   = "mun_trab",
  "cbo_2002_ocupacao"                = "cbo_ocupacao_2002",
  "ind_estabelecimento_participante_simples" = "ind_simples",
  "ind_estab_participante_simples"   = "ind_simples",
  "ind_estabelecimento_participante_pat" = "ind_estab_participa_pat",
  "ind_estab_participante_pat"       = "ind_estab_participa_pat",
  "ind_estab_participa_pat"          = "ind_estab_participa_pat",
  "tipo_admissao_trabalhador"        = "tipo_admissao",
  "tipo_estabelecimento"             = "tipo_estab",
  "tipo_estabelecimento___nome"      = "tipo_estab_1",
  "tipo_deficiencia"                 = "tipo_defic",
  "ind_trabalho_intermitente"        = "ind_trab_intermitente",
  "ind_trabalho_parcial"             = "ind_trab_parcial"
)

# columns that stay double precision when narrowing types
KEEP_DOUBLE_RX <- "^(vl_rem|tempo_emprego|qtd_hora_contr)"

normalize_header <- function(h) {
  h <- tolower(trimws(h))
  h <- remove_special_character(h)
  h <- gsub("[^[:alnum:][:space:]]", "_", h)
  gsub("[[:space:]]", "_", h)
}

#' Map raw RAIS headers (any dialect) to legacy contracted names
#' @keywords internal
map_headers <- function(raw) {
  n <- normalize_header(raw)
  n <- sub("___codigo$", "", n)   # strips the " - Codigo" suffix (new dialect)
  ifelse(n %in% names(new2legacy), new2legacy[n], n)
}

#' Guess the separator of a raw data file line
#' @keywords internal
guess_sep <- function(line) {
  if (is.na(line)) return(";")
  if (gregexpr(",", line)[[1]][1] == -1) return(";")
  if (gregexpr(";", line)[[1]][1] == -1) return(",")
  semis <- length(gregexpr(";", line, fixed = TRUE)[[1]])
  commas <- length(gregexpr(",", line, fixed = TRUE)[[1]])
  ifelse(commas > semis, ",", ";")
}

# fread() on .gz is broken under R >= 4.4 + R.utils (popTemporaryFile ->
# regexpr(NA)); decompress to a temp .txt with system gunzip and read that.
# Requires the gunzip binary (universal on Linux/macOS).
fread_safe <- function(path, ...) {
  if (!grepl("\\.gz$", path)) return(data.table::fread(path, ...))
  tmp <- tempfile(fileext = ".txt")
  status <- system2("gunzip", c("-c", shQuote(path)), stdout = tmp)
  if (!identical(status, 0L) || file.size(tmp) == 0)
    stop("falha ao descomprimir ", path)
  on.exit(unlink(tmp), add = TRUE)
  data.table::fread(tmp, ...)
}

read_head <- function(path, n = 1L) {
  con <- if (grepl("\\.gz$", path)) gzfile(path, "rt") else file(path, encoding = "LATIN1")
  on.exit(close(con), add = TRUE)
  readLines(con, n = n, warn = FALSE)
}

#' Ciclo completo RAIS -> PostgreSQL em 5 chamadas
#'
#' \preformatted{
#' cat   <- rais_catalog(years = 2026)                          # 1. cataloga no FTP
#' cat   <- rais_download(cat, dir = "rais_data", workers = 7)  # 2. baixa (paralelo)
#' cache <- rais_build(cat, dir = "rais_cache")                 # 3. converte p/ dialeto contratado
#' rais_pg_load(cache)                                          # 4. carrega no PostgreSQL
#' rais_create_indexes(2026)                                    # 5. indices
#' rais_narrow_types("rais_vinculo_2026")                       # (opcional) smallint/integer
#' }
#'
#' Credenciais vem das variaveis de ambiente compartilhadas com o AEDi:
#' `dbrais` (banco), `mte_rais` (usuario), `pwdrais` (senha), `hostraispsql`
#' (host). Contrato congelado de tabelas/colunas: ver [rais_pg_load()].
#'
#' @name raisqlr-cycle
NULL

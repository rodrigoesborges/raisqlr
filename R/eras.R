# Eras das tabelas rais_vinculo_YYYY / rais_estabelecimento_YYYY ----------------
#
# Contrato do banco carregado pelo raisqlr: os nomes de coluna variam com a
# epoca dos microdados. Eras verificadas por introspecao do mte_rais
# (2026-09-21; nao existe tabela 2003 - carga incompleta no FTP).
#
# vinculo:        2000-2003  cnae_95_classe (5d) + cbo_94_ocupacao (4-5d)
#                 2004-2005  cnae_95_classe + cbo_ocupacao_2002 (a coluna
#                            cnae_2_0_classe existe, mas veio zerada na carga)
#                 2006-      cnae_2_0_classe (5d, inclusive 2024-2025);
#                            cbo_ocupacao_2002 (5-6d)
# estabelecimento 2000-2001  clas_cnae_95 + tamestab
#                 2002-2005  cnae_95_classe + tamanho_estabelecimento
#                 2006-2023  + cnae_2_0_classe (5d)
#                 2024-      cnae_2_0_classe com DV (7d)
#
# Comprimentos verificados por introspecao do mte_rais (2026-09-21):
# vinculo tem classes de 4-5 digitos em TODOS os anos 2013-2025 e cnae_2_0
# populado apenas de 2006 (a coluna 2004-2005 veio zerada na carga); o DV
# de 7 digitos a partir de 2024 aparece apenas no estabelecimento. No
# estabelecimento 2024+ a coluna cnae_95_classe e vestigial (4 digitos) e
# por isso esta desabilitada (NA) no dicionario abaixo.
#
# colunas estaveis em todas as eras (nao listadas no dicionario):
# municipio, vinculo_ativo_31_12, vl_remun_dezembro_nom, vl_remun_media_nom.

.rais_eras <- data.frame(
  tabela = c(rep("vinculo", 3), rep("estabelecimento", 4)),
  ano_min = c(2000L, 2004L, 2006L, 2000L, 2002L, 2006L, 2024L),
  ano_max = c(2003L, 2005L, 2999L, 2001L, 2005L, 2023L, 2999L),
  era = c("cnae95-cbo94", "cnae95-cbo02", "cnae20-cbo02",
          "cnae95-clas-tamestab", "cnae95", "cnae20", "cnae20-dv"),
  coluna_cnae_2_0 = c(NA, NA, "cnae_2_0_classe", NA, NA,
                      "cnae_2_0_classe", "cnae_2_0_classe"),
  coluna_cnae_95  = c("cnae_95_classe", "cnae_95_classe", "cnae_95_classe",
                      "clas_cnae_95", "cnae_95_classe", "cnae_95_classe",
                      NA_character_),
  coluna_cbo_2002 = c(NA, "cbo_ocupacao_2002", "cbo_ocupacao_2002", NA, NA, NA, NA),
  coluna_cbo_94   = c("cbo_94_ocupacao", NA, NA, NA, NA, NA, NA),
  coluna_porte    = c(NA, NA, NA, "tamestab", "tamanho_estabelecimento",
                      "tamanho_estabelecimento", "tamanho_estabelecimento"),
  cnae_2_0_digitos = c(NA, NA, 5L, NA, NA, 5L, 7L)
)

#' Era de uma tabela da RAIS
#'
#' Identifica a era (padrao de nomes de coluna) de `rais_vinculo_YYYY` ou
#' `rais_estabelecimento_YYYY` no banco carregado pelo raisqlr. A tabela 2003
#' nao existe (carga incompleta no FTP).
#'
#' @param ano ano de referencia (a tabela `_<ano>`).
#' @param tabela `"vinculo"` ou `"estabelecimento"`.
#' @return string com o rotulo da era (ver `R/eras.R`).
#' @export
rais_era <- function(ano, tabela = c("vinculo", "estabelecimento")) {
  tabela <- match.arg(tabela)
  era <- .rais_eras[.rais_eras$tabela == tabela &
                    ano >= .rais_eras$ano_min & ano <= .rais_eras$ano_max, ]
  if (!nrow(era)) stop("ano fora do contrato do raisqlr: ", tabela, " ", ano)
  era$era
}

#' Coluna de um conceito na era/ano
#'
#' Devolve o nome da coluna que materializa o conceito (`"cnae_2_0"`,
#' `"cnae_95"`, `"cbo_2002"`, `"cbo_94"`, `"porte"`) na tabela do ano, ou
#' `NA_character_` quando a era nao oferece o conceito (ex.: CNAE 2.0 em
#' vinculos de 2000-2003). Conceitos estaveis em todas as eras
#' (`municipio`, `vinculo_ativo_31_12`, `vl_remun_dezembro_nom`) nao
#' precisam deste helper.
#'
#' @inheritParams rais_era
#' @param conceito um de `"cnae_2_0"`, `"cnae_95"`, `"cbo_2002"`, `"cbo_94"`,
#'   `"porte"`.
#' @return nome da coluna ou `NA_character_`.
#' @export
rais_coluna <- function(ano, tabela = c("vinculo", "estabelecimento"), conceito) {
  tabela <- match.arg(tabela)
  conceito <- match.arg(conceito,
    c("cnae_2_0", "cnae_95", "cbo_2002", "cbo_94", "porte"))
  era <- .rais_eras[.rais_eras$tabela == tabela &
                    ano >= .rais_eras$ano_min & ano <= .rais_eras$ano_max, ]
  if (!nrow(era)) stop("ano fora do contrato do raisqlr: ", tabela, " ", ano)
  col <- era[[paste0("coluna_", conceito)]]
  if (!length(col) || is.na(col)) NA_character_ else col
}

#' Divisor de `trunc()` para um nivel de CNAE
#'
#' Os codigos CNAE das tabelas variam de granularidade por era (5 digitos
#' no vinculo em todas as eras; no estabelecimento a subclasse vem com
#' digito verificador - 7 digitos - a partir de 2024). Este helper devolve o
#' divisor que extrai `nivel` da coluna CNAE do ano:
#' `trunc(coluna / rais_divisor(ano, tabela, nivel))`.
#'
#' @inheritParams rais_era
#' @param nivel `"divisao"` (2 digitos), `"grupo"` (3) ou `"classe"` (4).
#' @param conceito `"cnae_2_0"` (default; 7 digitos no estabelecimento a
#'   partir de 2024) ou `"cnae_95"` (5 digitos em todas as eras).
#' @return integer com o divisor (1000/100/10 ou 100000/10000/1000).
#' @export
rais_divisor <- function(ano, tabela = c("vinculo", "estabelecimento"),
                         nivel = c("divisao", "grupo", "classe"),
                         conceito = "cnae_2_0") {
  tabela <- match.arg(tabela)
  nivel <- match.arg(nivel)
  conceito <- match.arg(conceito, c("cnae_2_0", "cnae_95"))
  era <- .rais_eras[.rais_eras$tabela == tabela &
                    ano >= .rais_eras$ano_min & ano <= .rais_eras$ano_max, ]
  if (!nrow(era)) stop("ano fora do contrato do raisqlr: ", tabela, " ", ano)
  digitos <- if (conceito == "cnae_2_0") era$cnae_2_0_digitos else 5L
  if (is.na(digitos)) stop("conceito cnae_2_0 ausente na era ", era$era)
  as.integer(c(divisao = 10^(digitos - 2L), grupo = 10^(digitos - 3L),
    classe = 10^(digitos - 4L))[[nivel]])
}

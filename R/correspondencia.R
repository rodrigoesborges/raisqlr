# Correspondencia oficial CNAE 2.0 <-> CNAE 1.0 (95) em nivel de classe ------
#
# Dados: nota CONCLA/IBGE "Correspondencia CNAE 2.0 x CNAE 1.0" (gerada por
# data-raw/correspondencia_cnae.R a partir de CNAE20_Correspondencia20x10.xls).
# Cada classe 2.0 corresponde a 1+ classes 1.0 (asterisco = conteudo parcial);
# o flag `preferencial` e o codigo dominante (marca X da nota ou correspondencia
# unica/primeiro par), util para nao contar duas vezes o mesmo conteudo.

#' Equivalentes CNAE entre versoes da classificacao
#'
#' Traduz codigos CNAE (2.0 <-> 1.0/95) em nivel de divisao, grupo ou classe,
#' usando a correspondencia oficial CONCLA/IBGE. Pensado para montar filtros
#' setoriais sobre tabelas RAIS de eras diferentes: uma consulta escrita para
#' CNAE 2.0 (ex.: divisao 72, pesquisa e desenvolvimento) pode selecionar o
#' equivalente na era CNAE 95 (divisao 73). A correspondencia e aproximada nas
#' fronteiras de setor: com `preferencial = TRUE` (default) usa-se o conteudo
#' dominante (sem dupla contagem); com `FALSE` entram todos os pares,
#' incluindo conteudo parcial.
#'
#' @param codigos codigos CNAE na versao `de`, no nivel `nivel`
#'   (divisao: 2 digitos; grupo: 3; classe: 4).
#' @param de,para `"2.0"` ou `"1.0"` (aceita alias `"95"`).
#' @param nivel nivel dos codigos de entrada e saida.
#' @param preferencial usar apenas as correspondencias dominantes?
#' @return integer vector com os codigos equivalentes em `para`, nivel
#'   `nivel`, unicos e ordenados.
#' @examples
#' cnae_equivalentes(72, "2.0", "1.0")            # 73
#' cnae_equivalentes(84, "2.0", "95")             # 75
#' cnae_equivalentes(c(1, 2, 3), "2.0", "1.0")    # 1 2 5
#' cnae_equivalentes(75, "1.0", "2.0")            # 84
#' @export
cnae_equivalentes <- function(codigos, de = "2.0", para = "1.0",
                              nivel = c("divisao", "grupo", "classe"),
                              preferencial = TRUE) {
  nivel <- match.arg(nivel)
  norm <- function(v) switch(ifelse(v == "95", "1.0", v), "2.0" = "2.0", "1.0" = "1.0",
    stop("versao deve ser '2.0' ou '1.0'"))
  de <- norm(de); para <- norm(para)
  if (de == para) stop("de e para devem ser versoes diferentes")
  if (de == "1.0") {
    p <- data.frame(origem = cnae_2_0_para_1_0$classe_1_0,
                    destino = cnae_2_0_para_1_0$classe_2_0,
                    preferencial = cnae_2_0_para_1_0$preferencial)
  } else {
    p <- data.frame(origem = cnae_2_0_para_1_0$classe_2_0,
                    destino = cnae_2_0_para_1_0$classe_1_0,
                    preferencial = cnae_2_0_para_1_0$preferencial)
  }
  # classes da correspondencia tem 4 digitos: divisao = 2, grupo = 3
  divisor <- c(divisao = 100L, grupo = 10L, classe = 1L)[[nivel]]
  # expande o codigo de entrada para classes, casa pela classe de origem e
  # agrega o destino no nivel pedido (flag preferencial e por classe de
  # origem 2.0: cada classe tem exatamente um par preferencial)
  alvo <- as.integer(codigos)
  m <- p[trunc(p$origem / divisor) %in% alvo, ]
  if (preferencial) m <- m[m$preferencial, ]
  if (!nrow(m)) return(integer(0))
  as.integer(sort(unique(trunc(m$destino / divisor))))
}

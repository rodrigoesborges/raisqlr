# Gera sysdata.rda com a correspondencia oficial CNAE 2.0 -> CNAE 1.0 (classes)
# Fonte: nota tecnica CONCLA/IBGE "Correspondencia CNAE 2.0 x CNAE 1.0"
# (CNAE20_Correspondencia20x10.xls, aba principal + aba de preferenciais).
# Cada classe 2.0 pode ter 1+ classes 1.0 (asterisco = conteudo parcial);
# a aba PREFERENCIAIS indica o codigo 1.0 preferencial de cada classe 2.0.

library(readxl)

p <- file.path("data-raw", "CNAE20_Correspondencia20x10.xls")
stopifnot(file.exists(p))

# codigo sem o digito verificador apos o traco ("01.11-2" -> 111)
num <- function(x) suppressWarnings(as.integer(gsub("[^0-9]", "", sub("-.*$", "", x))))

# aba 1: pares 2.0 x 1.0 (com asteriscos de correspondencia parcial)
a1 <- as.data.frame(readxl::read_excel(p, sheet = 1, col_names = FALSE))
pares <- data.frame(
  classe_2_0 = num(a1[[1]]),
  classe_1_0 = num(a1[[3]]),
  parcial    = !is.na(a1[[4]]) & trimws(as.character(a1[[4]])) != ""
)
pares <- pares[!is.na(pares$classe_2_0) & !is.na(pares$classe_1_0), ]
pares <- unique(pares[order(pares$classe_2_0, pares$classe_1_0), ])

# aba 2: codigos preferenciais 2.0 -> 1.0 (codigo 2.0 preenchido para baixo;
# a marca "X" na 4a coluna indica o codigo 1.0 preferencial)
a2 <- as.data.frame(readxl::read_excel(p, sheet = 2, col_names = FALSE))
c2 <- num(a2[[1]]); c2[is.na(c2)] <- c2[which(!is.na(c2))[cumsum(!is.na(c2))]][is.na(c2)]
pref <- data.frame(
  classe_2_0 = c2,
  classe_1_0 = num(a2[[2]]),
  marca = trimws(as.character(a2[[4]]))
)
pref <- pref[!is.na(pref$classe_2_0) & !is.na(pref$classe_1_0), ]
pref <- pref[pref$marca == "X", c("classe_2_0", "classe_1_0")]

# marca "X" so aparece quando ha mais de uma opcao 1.0; correspondencia
# unica e preferencial por definicao; multi-opcao sem X -> primeiro par
# (deterministico). O filtro setorial usa TODOS os pares (conteudo parcial
# importa), o flag serve para reclassificacao linha a linha.
marcados <- paste(pref$classe_2_0, pref$classe_1_0)
n_op <- table(pares$classe_2_0)
pares$preferencial <- paste(pares$classe_2_0, pares$classe_1_0) %in% marcados |
  pares$classe_2_0 %in% names(n_op[n_op == 1])
sem_pref <- setdiff(unique(pares$classe_2_0), pares$classe_2_0[pares$preferencial])
if (length(sem_pref)) {
  prim <- pares[pares$classe_2_0 %in% sem_pref, ]
  prim <- prim[!duplicated(prim$classe_2_0), ]
  pares$preferencial[match(paste(prim$classe_2_0, prim$classe_1_0),
    paste(pares$classe_2_0, pares$classe_1_0))] <- TRUE
}

stopifnot(nrow(pares) > 900, all(tapply(pares$preferencial, pares$classe_2_0, any)))
cat("pares:", nrow(pares), "| preferenciais:", sum(pares$preferencial), "\n")
cat("classes 2.0 cobertas:", length(unique(pares$classe_2_0)), "\n")

cnae_2_0_para_1_0 <- pares
save(cnae_2_0_para_1_0, file = "R/sysdata.rda")

# raisqlr

Download dos microdados da RAIS (MTE) e carga em banco PostgreSQL.

Substitui os scripts `dev/build_dbrais.R` / `temp_consolida_catalogo_pgsql.R` /
`indices_dbrais.R` do `pndr_dashboard` por um pacote com as lições da transição
v0→v0.1 (ago/2026).

```r
# install.packages("remotes")
remotes::install_github("rodrigoesborges/raisqlr")

cat   <- raisqlr::rais_catalog(years = 2026)                          # 1. cataloga no FTP
cat   <- raisqlr::rais_download(cat, dir = "rais_data", workers = 7)  # 2. baixa (paralelo)
cache <- raisqlr::rais_build(cat, dir = "rais_cache")                 # 3. converte p/ dialeto contratado
raisqlr::rais_pg_load(cache)                                          # 4. carrega no PostgreSQL
raisqlr::rais_create_indexes(2026)                                    # 5. indices
raisqlr::rais_narrow_types()                                          # (opcional) smallint/integer
```

Credenciais: variáveis de ambiente compartilhadas com o AEDi — `dbrais`
(banco, default `mte_rais`), `mte_rais` (usuário), `pwdrais` (senha),
`hostraispsql` (host, default `localhost`).

## Contrato congelado

Tabelas `public.rais_vinculo_YYYY` e `public.rais_estabelecimento_YYYY`,
nomes de colunas legados (`municipio`, `vinculo_ativo_31_12`,
`cbo_ocupacao_2002`, ...) — consumo direto de `AEDi/R/upload_raispsql.R` e
dos scripts `coleta/*` do PNDR. O pacote muda TIPOS (via `rais_narrow_types`),
nunca nomes.

## Notas de campo (bugs que o pacote já trata)

- **Dialeto novo (RAIS 2024 definitiva em diante):** arquivos internos
  `*.COMT`, `sep=","`, `dec="."`, cabeçalhos `"X - Código"` → `rais_build()`
  detecta o dialeto e normaliza para o contrato legado (`sep=";"`, `dec="."`).
- **`fread(.gz)` quebrado** sob R ≥ 4.4 + R.utils 2.12.3 (`popTemporaryFile →
  regexpr(NA)`): leituras internas passam por gunzip em temporário
  (`fread_safe()`; requer o binário `gunzip`).
- **CBO inválido `'0000-1'`** (RAIS 2020): corrigido dentro do `rais_build()`.
- **Carga idempotente e transacional:** cada tabela é DROP+CREATE+COPY numa
  transação única (rollback em erro) — sem o `append=TRUE` cego do pipeline
  antigo, que duplicava anos ao catalogar `2024` e `2024 Parcial` juntos.
- **Tipagem:** numéricos entram como `double precision` (amostras de 1 linha
  não distinguem integer/double com segurança no COPY); `rais_narrow_types()`
  varre a tabela inteira e converte para `smallint`/`integer` (no banco local:
  620 GB → 325 GB).
- **Preliminares:** desde a RAIS 2025 o MTE publica o release preliminar na
  pasta `"<ano>"` (sem sufixo) — `situacao` no catálogo reflete o sufixo da
  pasta; confira o Comunicado. Preliminares cobrem só o setor privado
  (NJ ≥ 2000); setor público pode ser replicado do ano definitivo anterior.

## Status

0.0.0.9000 — esqueleto funcional: catálogo (FTP/local), download paralelo,
conversão de dialeto, carga PostgreSQL, índices, narrowing e layouts.
Testes com fixture real (arquivo NI) e ciclo completo contra banco local.

library(raisqlr)
fixture <- testthat::test_path("fixtures", "RAIS_VINC_PUB_NI.7z")
has_db <- nzchar(Sys.getenv("pwdrais"))

# sintetiza um .7z de vinculo legado (sep=';', dec=',', LATIN1) com a coluna
# municipio podendo carregar o placeholder '0000-1' dos arquivos IGNORADO
# da RAIS 1994-97 (bucket de UF ignorada do MTE)
make_legacy_vinc_7z <- function(archive_path, txt_name, municipios) {
  lines <- c("Munic\u00edpio;V\u00ednculo Ativo 31/12;CBO Ocupa\u00e7\u00e3o 2002",
             paste(municipios, "1", "12345", sep = ";"))
  csv <- file.path(dirname(archive_path), txt_name)
  writeLines(iconv(enc2utf8(lines), from = "UTF-8", to = "LATIN1"),
             csv, useBytes = TRUE)
  owd <- setwd(dirname(archive_path)); on.exit(setwd(owd), add = TRUE)
  archive::archive_write_files(archive_path, txt_name, format = "7zip")
  archive_path
}

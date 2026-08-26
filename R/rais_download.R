#' Baixa os arquivos da RAIS catalogados
#'
#' Copia os arquivos `.7z` do catalogo ([rais_catalog()]) para um diretorio
#' local (datavault), preservando a estrutura `<ano>/arquivo.7z`. Arquivos
#' ja presentes com o mesmo tamanho do FTP nao sao baixados de novo.
#' Para releases da pasta `"<ano> Parcial"` os arquivos vao para
#' `"<ano> parcial"/` para nao colidirem com os definitivos.
#'
#' @param catalog catalogo gerado por [rais_catalog()].
#' @param dir destino local (default `"rais_data"` no diretorio de trabalho).
#' @param workers numero de downloads simultaneos (default 1; o FTP do MTE
#'   limita ~50-70 KB/s por conexao, entao valores como 6-8 reduzem muito o
#'   tempo total).
#' @param quiet passa `quiet` ao download.
#'
#' @return o catalogo de entrada acrescido da coluna `local_file`.
#' @export
rais_download <- function(catalog, dir = "rais_data", workers = 1, quiet = TRUE) {
  stopifnot(is.data.frame(catalog), all(c("full_path", "year", "file", "situacao") %in% names(catalog)))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  sub <- ifelse(catalog$situacao == "parcial" & !grepl("parcial", catalog$folder, ignore.case = TRUE),
                "", ifelse(catalog$situacao == "parcial", " parcial", ""))
  folder <- paste0(catalog$year, sub)
  dest_dir <- file.path(dir, folder)
  catalog$local_file <- file.path(dest_dir, catalog$file)

  todo <- !(file.exists(catalog$local_file) &
            (is.na(catalog$size_bytes) |
             file.size(catalog$local_file) == catalog$size_bytes))
  for (d in unique(dest_dir[todo])) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  idx <- which(todo)
  if (length(idx)) {
    message(length(idx), " arquivo(s) a baixar (", round(sum(catalog$size_bytes[idx], na.rm = TRUE) / 1e9, 2), " GB)")
    if (workers <= 1) {
      for (i in idx) fetch_one(catalog$full_path[i], catalog$local_file[i], quiet)
    } else {
      cl <- parallel::makeCluster(min(workers, length(idx)))
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(cl, c("fetch_one"), environment())
      invisible(parallel::parLapply(cl, idx, function(i)
        fetch_one(catalog$full_path[i], catalog$local_file[i], quiet)))
    }
  } else message("todos os arquivos ja estao em ", normalizePath(dir))

  catalog
}

fetch_one <- function(url, dest, quiet) {
  tmp <- paste0(dest, ".part")
  ok <- tryCatch({
    curl::curl_download(url, tmp, mode = "wb", quiet = quiet)
    TRUE
  }, error = function(e) { message("FALHA ", url, ": ", conditionMessage(e)); FALSE })
  if (ok) ok <- file.rename(tmp, dest)
  ok
}

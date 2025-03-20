# Terras indígenas no Brasil

download_tis <- function(path) {
  url <- "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_amazonia_legal_poligonais&maxFeatures=10000&outputFormat=SHAPE-ZIP"
  message("Fazendo download...")
  if (!dir.exists(path)) dir.create(path)
  httr::GET(url, httr::write_disk(here::here(path, "tmp.zip")))
  utils::unzip(here::here(path, "tmp.zip"), exdir = path)
  fs::file_delete(here::here(path, "tmp.zip"))
}

compare_tis <- function(dir_new, dir_old) {
  if (!dir.exists(dir_new)) dir.create(dir_new)
  new <- sf::read_sf(dir_new)
  old <- sf::read_sf(dir_old)
  error <- testthat::capture_error({
    testthat::expect_equivalent(old, new)
  })
  if (is.null(error)) {
    message("TIs sem alterações")
    fs::dir_delete(dir_new)
    return(FALSE)
  } else {
    fs::dir_delete(dir_old)
    message("TIs atualizadas")
    return(TRUE)
  }
}

update_tis <- function(dir_new, dir_old) {
  download_tis(dir_new)
  compare_tis(dir_new, dir_old)
}

update_index <- function(desc) {
  index <- readr::read_csv("index.csv")
  index |>
    dplyr::mutate(
      ultima_atualizacao = dplyr::case_when(
        descricao == desc ~ lubridate::today(),
        TRUE ~ ultima_atualizacao
      )
    ) |>
    readr::write_csv("index.csv")
  rmarkdown::render("README.Rmd")
}

update_md <- function(path) {
  path |>
    readr::read_file() |>
    stringr::str_replace(
      "[0-9]{4}-[0-9]{2}-[0-9]{2}",
      as.character(lubridate::today())
    ) |>
    readr::write_file(path)
}


# Processo de atualização

arquivos_atuais <- fs::dir_ls("territorios/terras-indigenas/funai-tis", type = "directory")
arquivos_novos <- here::here("territorios/terras-indigenas/funai-tis", stringr::str_glue("shp_{lubridate::now()}"))

atualizado <- update_tis(arquivos_novos, arquivos_atuais)

if (atualizado) {
  update_index("`shp` terras indígenas")
  update_md("territorios/terras-indigenas/funai-tis/README.md")
}


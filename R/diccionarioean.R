#' Diccionario espacios con afectaciones normativas PDU 2013 - 2023
#'
#' @param filename Shapefile
#' @importFrom utils head
#' @return sf
#' @export
diccionario_ean <- function(filename) {
  # read shapefile
  shp <- sf::st_read(filename) |>
    janitor::clean_names()
  cv_names <- names(shp)
  colnames(shp) <- gsub("_", "", cv_names)
  # seleccionar columnas
  print(head(shp))

  # message(crayon::green(filename))
  message(crayon::red(toupper(gsub(
    ".shp",
    "",
    str_extract_after(
      string = filename,
      pattern = "/",
      which = "last"
    )
  ))))

  zre <- readline(prompt = "Ingresar nombre de la ZRE:")
  codigo <- readline(prompt = "Ingresar codigo. 1 para ZPA Y 2 para ZPCE:")
  if (codigo == 1) {
    codigo <- "ZPA"
  } else if (codigo == 2) {
    codigo <- "ZPCE"
  }

  out <- shp |>
    dplyr::select(geometry)

  polygon_area <- sf::st_area(out)

  output <- out |>
    dplyr::mutate(
      zre = paste0("ZRE", zre) |> toupper(),
      codigo = codigo,
      documento = "DIAGNÓSTICO",
      doc_reglam = "PE",
      fuente = "PDU 2013 - 2023",
      area_m2 = polygon_area,
      area_ha = area_m2 / 10000L
    ) |>
    dplyr::mutate(nombre = dplyr::case_when(
      codigo == "ZPA" ~ "ZONA DE PROTECCION AMBIENTAL",
      codigo == "ZPCE" ~ "ZONA DE PROTECCION Y CONSERVACION ECOLOGICA"
    )) |>
    dplyr::mutate(distrito = dplyr::case_when(
      stringr::str_detect(zre, "CU") ~ "CUSCO",
      stringr::str_detect(zre, "SA") ~ "SANTIAGO",
      stringr::str_detect(zre, "SS") ~ "SAN SEBASTIAN",
      stringr::str_detect(zre, "SJ") ~ "SAN JERONIMO"
    )) |>
    dplyr::select(
      nombre, codigo, zre, distrito, documento,
      doc_reglam, fuente, area_m2, area_ha
    ) |>
    dplyr::mutate_if(is.character, ~ toupper(.) |>
      stringr::str_squish() |>
      stringr::str_trim()) |>
    dplyr::relocate(geometry, .after = dplyr::last_col())
  print(head(output))
  full_names <- c(
    "nombre", "codigo", "zre", "distrito", "documento",
    "doc_reglam", "fuente", "area_m2", "area_ha", "geometry"
  )

  if (length(names(output)) == length(full_names)) {
    message(crayon::green("Informacion completa"))
  } else {
    message(crayon::green("Es necesario añadir informacion"))
    # head(output)
  }
}

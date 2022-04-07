#' Diccionario de datos cobertura vegetal
#'
#' @param filename sf path
#'
#' @return objecto de tipo sf
#' @export
diccionario_cv <- function(filename) {
  # importar los shapefiles
  sf::st_read(filename) |>
    sf::st_zm(drop = TRUE, what = "ZM") |>
    janitor::clean_names() -> shp
  # eliminar subguiones de los nombres de las columnas
  cv_names <- names(shp)
  colnames(shp) <- gsub("_", "", cv_names)
  # mostrar el encabezado de las columnas

  print(head(shp))

  message(crayon::red(toupper(gsub(
    ".shp",
    "",
    str_extract_after(
      string = filename,
      pattern = "/",
      which = "last"
    )
  ))))
  # ingresar código de las zona de reglamentación
  zre <- readline(prompt = "Seleccionar la variable ZRE:")
  if (!grepl("[a-z][0-9]", zre)) {
    zre <- NA_real_
  }
  # seleccionar el nombre de la columna que contiene la información
  # del tipo de cobertura NATURAL - ANTROPICA
  tipo_cober <- readline(prompt = "Seleccionar la variable TIPO COBERTURA:")
  if (!grepl("^[a-z]+$", tipo_cober)) {
    tipo_cober <- NA_real_
  }
  # seleccionar el nombre de la columna que contiene la información
  # de los tipos de cobertura vegetal
  # ARBOREA - HERBAZAL - MATORRAL- PASTIZAL...
  cobert_veg <- readline(prompt = "Seleccionar la variable COBERTURA VEGETAL:")
  if (!grepl("^[a-z]+$", cobert_veg)) {
    cobert_veg <- NA_real_
  }
  # seleccione de columnas presentes en el shapefile
  vars <- c(tipo_cober, cobert_veg)
  names(vars) <- c("tipo_cober", "cobert_veg")
  vars1 <- vars[!is.na(vars)]
  names_vars1 <- c(names(vars1), "geometry")
  # recuperar columnas presentes en el shp
  out <- shp |>
    dplyr::select(
      dplyr::all_of(vars1),
      geometry
    )
  # renombrar las columnas
  colnames(out) <- names_vars1
  # calcular el área de los polígonos
  polygon_area <- sf::st_area(out)
  #
  vars_2 <- names(out)

  output <- out |>
    dplyr::mutate(
      zre = paste0("ZRE", zre) |> toupper(),
      documento = "DIAGNÓSTICO",
      doc_reglam = "PE",
      fuente = "TRABAJO DE CAMPO",
      area_m2 = polygon_area,
      area_ha = area_m2 / 10000L
    ) |>
    dplyr::mutate_if(is.character, ~ toupper(.) |>
      stringr::str_squish() |>
      stringr::str_trim()) |>
    dplyr::mutate(distrito = dplyr::case_when(
      stringr::str_detect(zre, "CU") ~ "CUSCO",
      stringr::str_detect(zre, "SA") ~ "SANTIAGO",
      stringr::str_detect(zre, "SS") ~ "SAN SEBASTIAN",
      stringr::str_detect(zre, "SJ") ~ "SAN JERONIMO",
    )) |>
    dplyr::select(
      dplyr::all_of(vars_2), zre, distrito, documento,
      doc_reglam, fuente, area_m2, area_ha
    ) |>
    dplyr::relocate(geometry, .after = dplyr::last_col())

  # validar la información necesaria, con base en el diccionario de datos
  nombres_cv <- c(
    "tipo_cober", "cobert_veg", "zre", "distrito", "documento",
    "doc_reglam", "fuente", "area_m2", "area_ha", "geometry"
  )

  # validar los niveles de cobertura vegetal
  niveles_cv <- c(
    "ARBÓREA", "MATORRAL", "HERBAZAL", "PASTIZAL",
    "ESCASA COBERTURA", "ZONA URBANA"
  )

  if (length(names(output)) == 10) {
    return(output)
  } else if (length(names(output)) == 9) {
    "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
    message(crayon::green(paste(
      "Es necesario añadir informacion!!!",
      nombres_cv %w/o% names(output)
    )))
    return(output)
  }
}
